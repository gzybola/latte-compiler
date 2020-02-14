module ASB where

import Absfvc
import Control.Monad.Reader
import Data.Map as Map
import Control.Monad.State
import Data.Data
import Data.Typeable
import Data.Char
import Control.Monad.Identity
import Data.List
import Data.Hex
import StringGen
import Data.Map as M
import AbsLatte (Type(Int, Str))
import Control.Exception (assert)

type Compiler = ReaderT CompilerEnv (State CompilerState)

data Stack = Minus | Plus deriving (Typeable, Data)

data CompilerState  = CS {
    prog :: [String], 
    emptyRegisters :: [Reg],
    lastReg :: Reg,
    lastIden :: String,
    strs :: M.Map String String,
    regs :: Map.Map String Reg,
    mem :: Integer
}

data CompilerEnv = IE {
    vars :: Map.Map String Arg,
    counter :: Integer
}

initCS = CS {
    prog = [],
    emptyRegisters = registersInUse,
    lastReg = Edi,
    lastIden = "",
    strs = M.empty,
    regs = M.empty
}

registersInUse = [Edi, Esi, Ebx]


initEnv = IE {
    vars = Map.empty,
    counter = 1
}

resReg = Eax

data Reg = Eax | Ebp | Esp | Esi | Edi | Edx | Ecx | Ebx deriving (Typeable, Data, Eq) 

instance Show Reg where
  show = (\t -> "%" ++ (Prelude.map toLower t)) . showConstr . toConstr

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
data Arg = R Reg | L Stack Integer | Num Integer | Top

instance Show Arg where
    show (L Minus num) = (show ((-4)*num)) ++ "(%ebp)"
    show (L Plus num)  = (show (4*num)) ++ "(%ebp)"
    show (Num n)     = "$" ++ (show n)
    show (R reg)     = show reg
    show Top         = "(%esp)"
    
suffix = "l"
tab = "        "

data AOp = Leave | Neg Arg | Cmp Arg Arg | Jmp Label | Cdq | CJmp RelOp_ Label | Mov Arg Arg | Push Arg | Call Iden | Ret | Lea String Reg | Pop Arg | B ArithOp Arg Arg | S ArithOp Arg

instance Show AOp where 
  show (Mov arg1 arg2) = tab ++ "mov" ++ suffix ++ " " ++ (show arg1) ++ ", " ++ (show arg2)
  show (B op arg1 arg2) = tab ++ (show op) ++ " " ++ (show arg1) ++ ", " ++ (show arg2)
  show Cdq = tab ++ "cdq"
  show (Neg arg) = tab ++ "neg" ++ suffix ++ " " ++ (show arg)
  show (S op arg1) = tab ++ (show op) ++ " " ++ (show arg1)
  show (Push arg) = tab ++ "push" ++ suffix ++ " " ++ (show arg)
  show (Call id) = tab ++ "call " ++ show id
  show Ret = tab ++ "ret"
  show Leave = tab ++ "leave"
  show (Jmp id) = tab ++ "jmp ." ++ id
  show (Pop arg) = tab ++ "pop" ++ suffix ++ " " ++ (show arg)
  show (Lea str reg) = tab ++ "lea" ++ suffix ++ " " ++ str ++ ", " ++ (show reg)
  show (Cmp a b) = tab ++ "cmp" ++ " " ++ (show a) ++ ", " ++ (show b)
  show (CJmp op id) = tab ++ (show op) ++ " ." ++ id

clearRegister :: String -> Compiler ()
clearRegister id = do
    (regs, lastIden) <- gets(\env -> ((regs env), (lastIden env)))
    let nextRegs = Map.delete id regs
    let nextIden = if lastIden == id then ""
                 else lastIden
    modify(\env -> env {regs = nextRegs, lastIden = nextIden})

changeLocVar :: Iden -> Arg -> Compiler (Arg, CompilerEnv)
changeLocVar (DecId t str) arg = do
    (vars, number) <- asks(\env -> (vars env,counter env))
    let (stack, nextNum) = if t == Str then (8, number + 2)
                  else (4, number + 1) 
    emit (show (Mov arg (L Minus number)))
    return ((L Minus number), (IE (Map.insert str (L Minus number) vars) nextNum))

changeLocVar (LocId t str) arg = do                 
    clearRegister str
    loc <- asks(\env -> (vars env) Map.! str)
    emit(show (Mov arg loc))
    env <- ask
    return (loc, env)  
changeLocVar (Iden t str) arg = changeLocVar (DecId t str) arg

emit :: String -> Compiler ()
emit line = modify(\env -> env {prog = (line):(prog env)})

clearRegisters :: Compiler ()
clearRegisters = do
    reg <- gets(\env -> (lastReg env))
    let freeR = [ x | x <- registersInUse, x /= reg ]
    modify (\env -> env {emptyRegisters = registersInUse, regs = Map.fromList [((lastIden env), reg)]})

freeRegister :: Compiler Reg
freeRegister = do
    reg <- gets (\env -> emptyRegisters env)
    case reg of
        []      -> clearRegisters >> freeRegister
        (reg:l) -> do
            modify (\env -> env {emptyRegisters = l}) >> return reg

changeLastReg :: Reg -> Iden -> Compiler ()
changeLastReg reg iden = 
    modify(\env -> env {lastIden = (show iden), lastReg = reg, regs = Map.insert (show iden) reg (regs env)})

setFreeRegister :: Iden -> Compiler Reg
setFreeRegister (LocId t str) = do
    reg <- freeRegister
    loc <- asks(\e -> (vars e) Map.! str)
    emit(show $ Mov loc (R reg))
    changeLastReg reg (LocId t str)  
    return reg

setFreeRegister (DecId t str) = setFreeRegister (LocId t str)

setFreeRegister (Iden t str) = setFreeRegister (LocId t str)

atomToArg :: Atom -> Compiler Arg
atomToArg (AtomConst n) = return $ Num n
atomToArg (AtomVar id) = do
    maybeReg <- gets(\env -> Map.lookup (show id) (regs env))
    case maybeReg of
        (Just r) -> return (R r)
        Nothing -> setFreeRegister id >>= return . R 
atomToArg (AtomStr str) = do
    reg <- freeRegister
    changeLastReg reg (Emmm)
    name <- gets(\env -> (strs env) Map.! str)
    emit(show (Lea name reg))
    return (R reg)

atomToCArg :: Atom -> Compiler Arg
atomToCArg (AtomConst n) = do 
  emit (show(Push (Num n)))
  return (Top)    
atomToCArg d = atomToArg d

clearToCArg :: Atom -> Compiler ()
clearToCArg (AtomConst _) = do 
  emit (show $ B AddArithOp (Num 4) (R Esp))
clearToCArg _ = return ()

atomToReg :: Atom -> Reg -> Compiler ()
atomToReg (AtomConst n) reg = do
    emit (show $ Mov (Num n) $ R reg)
atomToReg (AtomVar id) reg = do
    arg <- atomToArg (AtomVar id)
    emit (show $ Mov arg $ R reg)
    let name = case id of 
          (DecId _ str)  -> str
          (LocId _ str)  -> str
          (Iden _ str)   -> str
    return ()

atomToReg (AtomStr str) reg = do
    name <- gets(\env -> (strs env) Map.! str)
    emit(show (Lea name reg))
    
changeIden :: Iden -> Arg -> Compiler (Arg, CompilerEnv)
changeIden id arg = changeLocVar id arg

addArg :: Atom -> Compiler ()
addArg atom = do
    arg <- atomToArg atom
    emit(show $ Push arg) 

compileNJS :: NonJmpStmt -> Compiler CompilerEnv 
compileNJS StmtEmpty = ask >>= return 
compileNJS (StmtNoOp id atom) = do 
    arg <- atomToArg atom
    clearRegister $ show id 
    (_, env) <- changeIden id arg
    return env
compileNJS (StmtNegOp id atom) = do
    arg <- atomToArg atom
    emit(show (Neg arg))
    changeIden id arg
    ask >>= return

compileNJS (StmtBinOp id atom1 DivArithOp atom2) = do
    reg <- atomToCArg atom2
    atomToReg atom1 Eax
    emit(show Cdq)
    emit(show (S DivArithOp reg))
    clearToCArg atom2
    (_, env) <- changeIden id $ R resReg
    return env

compileNJS (StmtBinOp id atom1 ModArithOp atom2) = do
    reg <- atomToCArg atom2
    atomToReg atom1 Eax
    emit(show Cdq)
    emit(show (S DivArithOp reg))
    clearToCArg atom2
    (_, env) <- changeIden id $ R Edx
    return env

compileNJS (StmtBinOp id atom1 ConcatArithOp atom2) = 
    compileNJS (StmtCallRet id (Iden Int (show ConcatArithOp)) [atom2, atom1])

compileNJS (StmtBinOp id atom1 op atom2) = do
    arg1 <- atomToArg atom2 
    atomToReg atom1 Eax 
    emit(show $ B op arg1 (R Eax))
    (arg, env) <- changeIden id (R Eax)
    return env 

compileNJS (StmtCall id atoms) = do
    mapM_ addArg atoms 
    emit (show $ (Call id))
    let mem = toInteger $ length atoms
    emit (show $ B AddArithOp (Num (4*mem)) (R Esp))
    ask >>= return

compileNJS (StmtCallRet res id atoms) = do
    compileNJS (StmtCall id atoms)
    (_, env) <- changeIden res (R Eax)
    return env

compileJS :: JmpStmt -> Compiler ()
compileJS StmtVRet = do
    mapM_ (\reg -> emit(show(Pop (R reg)))) (reverse registersInUse)
    emit(show Leave)
    emit(show Ret)
compileJS (StmtRet atom) = atomToReg atom Eax >> compileJS StmtVRet
compileJS (StmtGoTo id) = emit (show $ Jmp id)     
compileJS (StmtCondJmp atom1 op atom2 id1 id2) = do
    arg2 <- atomToArg atom2
    atomToReg atom1 Eax
    emit(show (Cmp arg2 (R Eax)))
    emit(show (CJmp op id1))
    emit(show (Jmp id2))


getString :: Atom -> Maybe String
getString (AtomStr str) = Just ( str)
getString _             = Nothing

blockName :: Label -> Label
blockName id = "." ++ id ++":"

compileCB :: CodeFlow -> Compiler ()
compileCB (Flow (CodeBlock id stmts jmp) res) = do
    emit(blockName id)
    envS <- ask 
    envR <- foldM (\env stm -> local (\_ -> env) (compileNJS stm)) envS stmts
    local (\_ -> envR) (compileJS jmp)
    modify (\env -> env {emptyRegisters = registersInUse, lastIden = "", regs = Map.empty})
    mapM_ (\f -> local (\_ -> envR) (compileCB f)) res 
compileCB Leaf = return ()


funName :: Iden -> String
funName id = (show id) ++ ":"

funProlog :: Integer -> Compiler ()
funProlog num = do
    emit (show (Push (R Ebp)))
    emit (show (Mov (R Esp) (R Ebp)))
    emit (show $ B SubArithOp (Num ((num + 4) * 4)) (R Esp))
    mapM_ (\reg -> emit $ show (Push (R reg))) registersInUse

addArgs :: [DataDef] -> CompilerEnv -> Integer -> CompilerEnv
addArgs [] env _ = env
addArgs (e:r) env n = addArgs r (env { vars = Map.insert (show e) (L Plus n) (vars env)}) (n+1) 

argumentNth = 2
type ArgCollector = State Integer

getTypeSize :: Type -> Integer
getTypeSize t = case t of
    Str -> 2
    _   -> 1

addVarNumber :: Integer -> ArgCollector()
addVarNumber i = do 
    number <- get
    put (number + 1)

stmtVar :: NonJmpStmt -> ArgCollector ()
stmtVar (StmtBinOp (Iden t _) _ _ _) = addVarNumber (getTypeSize t)
stmtVar (StmtNegOp (Iden t _) _) = addVarNumber (getTypeSize t)
stmtVar (StmtCallRet (Iden t _) _ _) = addVarNumber (getTypeSize t)  
stmtVar (StmtNoOp (Iden t _) _) = addVarNumber (getTypeSize t)
stmtVar (StmtNoOp (DecId t _) _) = addVarNumber (getTypeSize t)
stmtVar _  = return ()

getAllArgs :: CodeFlow -> ArgCollector ()
getAllArgs (Flow (CodeBlock id stmts jmp) res) = do
    mapM_ stmtVar stmts
getAllArgs Leaf = return ()

compileFP :: FourProgram -> Compiler ()
compileFP (FourProg type_ id args blocks) = do
  emit(funName id)
  let (_, stackArg) = (runState (getAllArgs blocks) 0)
  funProlog (stackArg + 5)
  let env = addArgs (reverse args) initEnv argumentNth
  modify (\en -> en {mem = stackArg})
  local (\_ -> env) (compileCB blocks)
  return () 

compile :: [FourProgram] -> Compiler String
compile prg = do 
    let (strProlog, mapStr) = allStrings prg
    modify(\env -> env {strs = mapStr})
    mapM_ compileFP prg
    asm <-gets (\env -> prolog ++ (intercalate "\n" (reverse (prog env))) ++ strProlog ++ "\n")
    return asm

runC :: [FourProgram] -> String
runC p = (evalState ((runReaderT (compile p) initEnv)) initCS)


prolog = "\
\.section .text\n.\
\type main, @function\n\
\.global main\n\
\"




