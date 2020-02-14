module FourValue where

import Control.Monad.Writer
import AbsLatte
import Absfvc
import qualified Data.Map as Map
import Data.Monoid(Endo)
import Control.Monad.Identity
import Control.Monad.State
import Data.ByteString.Internal (inlinePerformIO)
import System.IO.Unsafe
import Data.IORef

type Compiler b = State CompilerState b

data CompilerState = CS { counter :: Integer
                        , funEnv :: Map.Map Ident Type
                        , nonJmp :: [NonJmpStmt]
                        , codeBlock :: Map.Map Label CodeBlock
                        , currentLabel :: Label
                        , currentFun :: Ident
                        , jmps :: Map.Map Label JmpStmt
                        , types :: Map.Map Ident Type
                        , flow :: Map.Map Label [Label]
                        }

initialCS = CS { counter = 0
               , funEnv = Map.fromList initFuns
               , nonJmp = []
               , codeBlock = Map.empty
               , currentLabel = ""
               , jmps = Map.empty 
               , currentFun = Ident ""
               , types = Map.empty
               , flow = Map.empty
               }

initFuns = [((Ident "printInt"), Void)
           , ((Ident "printString"), Void)
           , ((Ident "error"), Void)
           , ((Ident "readInt"), Int)
           , ((Ident "readString"), Str)
           ]

-------------------------- Anicilary functions---------------------------
next :: Integer -> Integer
next a = a + 1

tmp :: Integer -> String
tmp int = ("_" ++  Prelude.show int)

newLabel :: Compiler Label
newLabel = do
  counter <- gets (\env -> counter env)
  modify (\env -> env {counter = counter + 1})
  return $ tmp (counter + 1)  

tmpVar :: Type -> Compiler Iden
tmpVar type_ = do
  counter <- gets (\env -> counter env)
  modify (\env -> env {counter = counter + 1})
  return (Iden type_ $ tmp (counter + 1)) 

tmpDec :: Type -> Compiler Iden
tmpDec t = do
  counter <- gets (\env -> counter env)
  modify (\env -> env {counter = counter + 1})
  return (DecId t $ tmp (counter + 1)) 

tmpLoc :: Iden -> Iden
tmpLoc (DecId t str) = (LocId t str)

findOp :: RelOp -> RelOp_
findOp LTH = LtRelOp
findOp LE  = LeRelOp
findOp GTH = GtRelOp
findOp GE  = GeRelOp
findOp EQU = EqRelOp
findOp NE  = NeRelOp

true = (AtomConst 1)
false = (AtomConst 0)

-------------------------Expresions-----------------------------------
compileExp_ :: ArithOp -> Expr -> Expr -> Compiler Atom
compileExp_ name_ exp1 exp2 = do
        atom1 <- compileExp exp1
        atom2 <- compileExp exp2
        let name = if ((name_ == AddArithOp) && ((atomType atom1) == Str)) then ConcatArithOp
                   else name_
        env <- get 
        let count = counter env
        put (env {counter = (next $ counter env)})
        let tmpVar = (Iden (atomType atom1) (tmp $ next count)) 
        let out = (StmtBinOp tmpVar atom1 name atom2)
        addNonJmpStmt out
        return $ AtomVar tmpVar 


compileExp :: Expr -> Compiler Atom 
compileExp (EAdd exp1 Plus exp2) = compileExp_ AddArithOp exp1 exp2
compileExp (EAdd exp1 Minus exp2) = compileExp_ SubArithOp exp1 exp2
compileExp (EMul exp1 Times exp2) = compileExp_ MulArithOp exp1 exp2
compileExp (EMul exp1 Div exp2) = compileExp_ DivArithOp exp1 exp2
compileExp (EMul exp1 Mod exp2) = compileExp_ ModArithOp exp1 exp2

compileExp (ELitInt int) = return $ AtomConst int 
compileExp ELitTrue = return $ AtomConst 1
compileExp ELitFalse = return $ AtomConst 0
compileExp (EVar (Ident id)) = do
  ttype <- gets(\env -> (types env) Map.! (Ident id))
  return $ AtomVar (LocId ttype id)
compileExp (EApp (Ident id) exps) = do
  atoms <- mapM compileExp exps
  returnT <- gets (\env -> (funEnv env) Map.! (Ident id))
  if returnT == Void then addNonJmpStmt (StmtCall (Iden Void id) atoms) >> return (AtomConst 0)
  else do 
    tmp <- tmpVar returnT 
    addNonJmpStmt (StmtCallRet tmp (Iden returnT id) atoms) 
    return (AtomVar tmp) 
   
compileExp (EString str) = return (AtomStr str)
compileExp (Neg exp) = do 
  atom <- compileExp exp
  case atom of
    (AtomConst x) -> return (AtomConst ((-1)*x))
    _             -> do
                     tmp <- tmpVar Int
                     addNonJmpStmt (StmtNegOp tmp atom)
                     return (AtomVar tmp)

compileExp (Not exp) = do
  atom <- compileExp exp
  tmp <- tmpVar Int
  addNonJmpStmt (StmtBinOp tmp (AtomConst 1) SubArithOp atom)
  return (AtomVar tmp)                     

compileExp (ERel exp1 op exp2) = do
  atom1 <- compileExp exp1
  atom2 <- compileExp exp2
  tmpTrue <- newLabel
  tmpFalse <- newLabel
  tmpEnd <- newLabel
  res <- tmpDec Int
  addNonJmpStmt (StmtNoOp res true)
  let mop = findOp op
  addCodeBlock (StmtCondJmp atom1 mop atom2 tmpTrue tmpFalse) [tmpTrue, tmpFalse]
  addLabel tmpTrue
  addCodeBlock (StmtGoTo tmpEnd) [tmpEnd]
  addLabel tmpFalse
  addNonJmpStmt (StmtNoOp (tmpLoc res) false)
  addCodeBlock (StmtGoTo tmpEnd) []
  addLabel tmpEnd
  return (AtomVar (tmpLoc res))
  
compileExp (EAnd exp1 exp2) = do
  atom1 <- compileExp exp1
  nextAnd <- newLabel
  inAnd <- newLabel
  notAnd <- newLabel
  afterAnd <- newLabel
  res <- tmpDec Int
  addNonJmpStmt (StmtNoOp res true)
  addCodeBlock (StmtCondJmp atom1 EqRelOp (AtomConst 0) notAnd nextAnd) [nextAnd, inAnd, notAnd]
  addLabel nextAnd
  atom2 <- compileExp exp2
  addCodeBlock (StmtCondJmp atom2 EqRelOp (AtomConst 1) inAnd notAnd) []
  addLabel inAnd
  addCodeBlock (StmtGoTo afterAnd) [afterAnd]
  addLabel notAnd
  addNonJmpStmt (StmtNoOp (tmpLoc res) false)
  addCodeBlock (StmtGoTo afterAnd) []
  addLabel afterAnd
  return (AtomVar (tmpLoc res))

compileExp (EOr exp1 exp2) = do
  atom1 <- compileExp exp1
  nextOr <- newLabel
  inOr <- newLabel 
  notOr <- newLabel
  afterOr <- newLabel
  addFlow [nextOr, notOr, inOr]
  res <- tmpDec Int
  addNonJmpStmt (StmtNoOp res false) 
  addCodeBlock (StmtCondJmp atom1 EqRelOp (AtomConst 1) inOr nextOr) [nextOr, notOr, inOr]
  addLabel nextOr
  atom2 <- compileExp exp2
  addCodeBlock (StmtCondJmp atom2 EqRelOp (AtomConst 1) inOr notOr) []
  addLabel notOr
  addCodeBlock (StmtGoTo afterOr) []
  addLabel inOr
  addNonJmpStmt (StmtNoOp (tmpLoc res) true)
  addCodeBlock (StmtGoTo afterOr) [afterOr]
  addLabel afterOr
  return (AtomVar (tmpLoc res))
 
---------------------------Statements-------------------------------
createDecStmt :: Item -> Type -> Compiler NonJmpStmt
createDecStmt (NoInit (Ident id)) t = do
  modify(\env -> env {types = Map.insert (Ident id) t (types env)})
  return (StmtNoOp (DecId t id) (AtomConst 0))
createDecStmt (Init (Ident id) exp) t = do
  modify(\env -> env {types = Map.insert (Ident id) t (types env)})
  at <-compileExp exp
  return (StmtNoOp (DecId t id) at)

tryAddFlow :: [Label] -> Compiler ()
tryAddFlow ids = do
  label <- gets(\env ->(currentLabel env))
  if "" == label then return ()
  else addFlow ids

addFlow :: [Label] -> Compiler ()
addFlow ids = do
  label <- gets(\env -> currentLabel env)
  modify(\env -> env {flow = Map.insert label ids (flow env)})

addNonJmpStmt :: NonJmpStmt -> Compiler ()
addNonJmpStmt st = modify (\env -> env {nonJmp = (st:(nonJmp env))})

addCodeBlock :: JmpStmt -> [Label] -> Compiler ()
addCodeBlock stmt ids = do
      (label, nonjmps) <- gets (\env -> (currentLabel env, reverse $ nonJmp env))
      tryAddFlow ids
      modify(\env -> env {nonJmp = [], currentLabel = "", 
                     codeBlock = (Map.insert label (CodeBlock label nonjmps stmt) (codeBlock env))})

addLabel :: Label -> Compiler ()
addLabel id = modify (\env -> env {currentLabel = id})
 
compileStmt :: Stmt -> Compiler ()
compileStmt Empty = return ()
compileStmt (BStmt (Blk stmts)) = do
  envS <- get
  block <- newLabel
  afterBlock <- newLabel
  addCodeBlock (StmtGoTo block) [block, afterBlock]
  addLabel block
  mapM_ compileStmt stmts
  addCodeBlock (StmtGoTo afterBlock) []
  addLabel afterBlock
  modify(\env -> env {types = (types envS)})

compileStmt (Decl t (item:r))   = createDecStmt item t >>= addNonJmpStmt >> compileStmt (Decl t r)
compileStmt (Decl _ []) = return ()
compileStmt (Ass (Ident id) exp) = do
                        atom <- compileExp exp
                        addNonJmpStmt (StmtNoOp (LocId (atomType atom) id) atom)
compileStmt (Incr (Ident id)) = addNonJmpStmt 
                               (StmtBinOp (LocId Int id) (AtomVar (LocId Int id)) AddArithOp (AtomConst 1))
compileStmt (Decr (Ident id)) = addNonJmpStmt
                               (StmtBinOp (LocId Int id) (AtomVar (LocId Int id)) SubArithOp (AtomConst 1))
compileStmt (Ret exp) = do
  atom <- compileExp exp
  addFlow [] 
  addCodeBlock (StmtRet atom) []
  after <- newLabel
  addLabel after
  
compileStmt VRet = do
  addCodeBlock StmtVRet []
  after <- newLabel
  addLabel after

compileStmt (SExp exp) = compileExp exp >> return ()

compileStmt (Cond expr stmt) = do
  inIf <- newLabel
  afterIf <- newLabel
  atom <- compileExp expr
  addCodeBlock (StmtCondJmp atom EqRelOp (AtomConst 1) inIf afterIf) [inIf, afterIf]
  addLabel inIf
  compileStmt stmt
  addCodeBlock (StmtGoTo afterIf) []
  addLabel afterIf

compileStmt (CondElse exp1 stmt1 stmt2) = do
  inIf <- newLabel
  inElse <- newLabel
  after <- newLabel
  atom <- compileExp exp1
  addCodeBlock (StmtCondJmp atom EqRelOp (AtomConst 1) inIf inElse) [inIf, inElse, after]
  addLabel inIf
  compileStmt stmt1
  addCodeBlock (StmtGoTo after) []
  addLabel inElse
  compileStmt stmt2
  addCodeBlock (StmtGoTo after) []
  addLabel after

compileStmt (While exp stmt) = do
  inwhile <- newLabel
  afterwhile <- newLabel
  beforewhile <- newLabel
  addCodeBlock (StmtGoTo beforewhile) [beforewhile]
  addLabel beforewhile 
  atom <- compileExp exp
  addCodeBlock (StmtCondJmp atom EqRelOp (AtomConst 1) inwhile afterwhile) [inwhile, afterwhile]
  addLabel inwhile
  compileStmt stmt
  addCodeBlock (StmtGoTo beforewhile) []
  addLabel afterwhile  

compileStmt (SExp exp) = compileExp exp >> return ()
------------------------Tops----------------------------------------
createFlow :: Label -> Map.Map Label CodeBlock -> Map.Map Label [Label] -> CodeFlow
createFlow id blocks flow = case block_ of
                                (Just b) -> Flow b fls
                                Nothing  -> Leaf 
  where
    block_ = Map.lookup id blocks
    fids = Map.lookup id flow
    fls = case fids of
      (Just f) -> (map (\id_ -> createFlow id_ blocks flow) f) 
      Nothing  -> []

convertFun :: TopDef -> Compiler FourProgram 
convertFun (FnDef tye (Ident id) args block) = do    
       let ttypies = Map.fromList $ map argDefT args
       modify (\env -> env { types = ttypies, currentLabel = id, currentFun = (Ident id), codeBlock=Map.empty}) 
       compileStmt (BStmt block)
       addCodeBlock (StmtVRet) []       
       let input = map argDef args
       (block, flow) <- gets (\env -> (codeBlock env, flow env))
      --  return (FourProg tye (Iden tye (id++ (show block))) input Leaf) 
       let tree = createFlow id block flow 
       modify (\env -> env {codeBlock = Map.empty, nonJmp = [], flow=Map.empty})
       return (FourProg tye (Iden tye (id)) input tree) 

addAllTopDef :: [TopDef] -> Compiler ()
addAllTopDef [] = return ()
addAllTopDef ((FnDef ftype (Ident id) _ _):k) = do
  env <- get
  modify (\env -> env {funEnv = Map.insert (Ident id) ftype (funEnv env)})
  addAllTopDef k

argDefT :: Arg -> (Ident, Type)
argDefT (Ar Bool id) = (id, Int)  
argDefT (Ar Str id) = (id, Str)
argDefT (Ar Int id) = (id, Int)

argDef :: Arg -> DataDef
argDef (Ar Bool (Ident id)) = BoolDataDef (Iden Int id)  
argDef (Ar Str (Ident id)) = StrDataDef (Iden Str id)
argDef (Ar Int (Ident id)) = IntDataDef (Iden Int id)

convert :: Program -> Compiler [FourProgram] 
convert (Prog l) =  do
  addAllTopDef l
  res <- mapM (\a -> convertFun a) l
  return res

runConvert :: Program -> FProgram
runConvert p = (evalState (convert p) initialCS)
