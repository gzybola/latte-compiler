module LLVM where

import Absfvc
import AbsLatte
import Data.List
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.State


type Compiler = State CompilerState

data CompilerState = CSt { counter :: Integer
                         , registers :: Map.Map Iden String
                         , p :: [String]
                         , strings :: Map.Map String Iden
                         , fstr :: [String]
                         }
initP = [ "declare i8* @concat(i8*, i8*)"]
initCS = CSt { counter = 0, registers = Map.empty, p = initP, strings = Map.empty, fstr = [] } 

addOp :: String -> Compiler ()
addOp wyr = modify (\env -> env { p = ((wyr):(p env)) } )

tmpVar :: Compiler Iden
tmpVar = do
  counter <- gets (\env -> counter env)
  modify (\env -> env {counter = counter + 1})
  return (Iden $ tmp (counter + 1))

tmp :: Integer -> String
tmp int = ("__" ++  Prelude.show int)

registerName :: Iden -> Compiler String
registerName (Iden str) = return $ "%" ++ str
--registerName (LocId str) = atom ( 

functionName :: Iden -> String
functionName (Iden str) = "@" ++ str

argName :: Atom -> Compiler String
argName atomm = atom atomm >>= (\name -> return $ (type_ atomm) ++ " " ++ name)

dataName :: DataDef -> String
dataName (IntDataDef (Iden id)) = "i32 " ++ id
dataName (BoolDataDef (Iden id)) = "i32 " ++ id
dataName (StrDataDef (Iden id)) = "i8* " ++ id

atom :: Atom -> Compiler String
atom (AtomConst int) = return $ Prelude.show int
atom (AtomVar type_ (Iden str)) = return $ "%" ++ str
atom (AtomVar type_ (LocId str)) = do
  let aa = if type_ == Str then "*"
           else ""
  tmp <- tmpVar
  ptr <- gets(\env -> (registers env) Map.! (LocId str))
  addOp ("%" ++ show tmp ++ " = load i32" ++ aa++", i32*" ++ aa ++ " " ++ ptr)
  return $ "%" ++ (show tmp)   
 
 
atom (AtomStr str) = do
  tmp <- tmpVar
  name <- registerName tmp
  cons <- gets(\env -> (strings env) Map.! str)
  addOp (name ++ " = bitcast [" ++ (show (length str + 1)) ++ " x 18]* @" ++ show cons ++ " to i8*" )
  return name 

type_ :: Atom -> String
type_ (AtomStr _) = "i8*"
type_ (AtomVar Str _) = "i8*"
type_ _ = "i32"

pointerLookup :: Iden -> Type -> Compiler String
pointerLookup (Iden str) _ = return $ "%" ++ str
pointerLookup id typee = do
  register <- gets (\env -> Map.lookup id (registers env))
  let addds =  if typee == Str then "*"
                else ""
  case register of 
    Nothing -> do
               let name = "%" ++ (show id) ++ "_ptr"
               addOp (name ++ " = alloca " ++ "i8" ++ addds)
               modify (\env -> env { registers = Map.insert id name (registers env)})
               return name
    (Just name) -> return name

ttype :: Type -> String
ttype Str = "i8*"
ttype _ = "i32"

compileNJS :: NonJmpStmt -> Compiler ()
compileNJS (StmtBinOp id (AtomStr str) AddArithOp atomm) = do 
  name <- registerName id
  atommm <- atom (AtomStr str)
  atommm2 <- atom atomm
  addOp (
compileNJS (StmtBinOp id (AtomVar Str id) AddArithOp atom)
compileNJS (StmtBinOp id atom1 op atom2) = do
  register <- registerName id
  atom1Name <- atom atom1
  atom2Name <- atom atom2
  addOp (intercalate " " [register, "=", show op, type_ atom1, atom1Name, ",", atom2Name])
compileNJS (StmtNegOp id atom) = compileNJS (StmtBinOp id (AtomConst 0) SubArithOp atom)
compileNJS (StmtCall id atoms) = do
  atomsNames <- mapM argName atoms
  addOp ((intercalate " " ["call", "void", functionName id, "("])
         ++
         (intercalate ", " atomsNames)
         ++ ")")
compileNJS (StmtCallRet ttype_ id fun atoms) = do
  atomsNames <- mapM argName atoms
  result <- registerName id
  addOp ((intercalate " " [result, "=", "call", ttype ttype_, functionName fun, "("])
         ++
         (intercalate ", " atomsNames)
         ++ ")")  
 
compileNJS StmtEmpty = return ()
compileNJS (StmtNoOp id atomm) = do
  name <- atom atomm
  pointer <- pointerLookup id $ atomType atomm
  addOp (intercalate " " ["store", type_ atomm, name, ",", (type_ atomm) ++ "*",
        pointer])

compileNJS (Phi id atom1 iden1 atom2 iden2) = do
  result <- registerName id
  sAtom1 <- atom atom1
  sAtom2 <- atom atom2
  idNa <- registerName iden1
  idNa2 <- registerName iden2  
  addOp (intercalate " " [result, "=", "phi", type_ atom1, "[", sAtom1, ",", idNa, "]",
                         ",[", sAtom2, ",    ", idNa2, "]"])

compileJS :: JmpStmt -> Compiler ()
compileJS (StmtGoTo id) = do
  name <- registerName id
  addOp (intercalate " " ["br", name])

compileJS (StmtRet atomm) = do 
  atommm <- atom atomm
  addOp (intercalate " " ["ret", atommm])

compileJS StmtVRet = addOp "ret void"

compileJS (StmtCondJmp atom1 op atom2 id1 id2) = do
  let t = type_ atom1 
  atomm1 <- atom atom1
  atomm2 <- atom atom2
  tmp <- tmpVar
  id1N <- registerName id1
  id2N <- registerName id2
  tmpName <- registerName tmp 
  addOp (intercalate " " [tmpName, "= icmp", show op, t, atomm1, ",", atomm2]) 
  addOp (intercalate " " ["br i1", tmpName, ",", "label", id1N, ",", "label", id2N])  
  
compileCB :: CodeBlock -> Compiler ()
compileCB (CodeBlock (Iden id) stmts stmt) = do
  addOp(id ++ ":")
  mapM compileNJS stmts
  compileJS stmt

checkA :: Atom -> Compiler ()
checkA atom = do
  case atom of
    (AtomStr str) -> modify (\env -> env { fstr = ((str):(fstr env)) } )
    _ -> return ()

findSStmt :: NonJmpStmt -> Compiler ()
findSStmt (StmtBinOp _ atommm1 _ atommm2) = checkA atommm1 >> checkA atommm2
findSStmt (StmtNegOp _ atommm) = checkA atommm
findSStmt (StmtCall _ atoms) = mapM_ checkA atoms
findSStmt (StmtCallRet _ _ _ atoms) = mapM_ checkA atoms 
findSStmt (StmtEmpty) = return ()
findSStmt (StmtNoOp _ aaatom) = checkA aaatom 
findSStmt (Phi _ aatom1 _ aatom2 _) = checkA aatom1 >> checkA aatom2 


findStr :: CodeBlock -> Compiler ()
findStr (CodeBlock _ stmts _) = do
  mapM_ findSStmt stmts
  
findFunStr :: FourProgram -> Compiler ()
findFunStr (FourProg _ _ _ block) = mapM_ findStr block 

compileFun :: FourProgram -> Compiler ()
compileFun (FourProg tttype id args block) = do
  let argsNmes = map dataName args
  addOp ((intercalate " " ["define", ttype tttype, functionName id, "("]) 
        ++ (intercalate ", " argsNmes) ++ ") {")
  mapM_ compileCB block
  addOp "}"  

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

addStr :: String -> Compiler ()
addStr str = do
  tmp <- tmpVar
  modify (\env -> env {strings = Map.insert str tmp (strings env)})
  addOp ("@" ++ show tmp ++ " = private constant [" ++ (show (length str + 1)) ++ " x 18] c\"" 
         ++ str ++ "\\00\"")


compile :: [FourProgram] -> Compiler String
compile prog = do
  mapM findFunStr prog 
  strs <- gets (\env -> rmdups $ fstr env)
  mapM addStr strs  
  mapM compileFun prog
  prog <- gets(\env -> p env)
  return $ intercalate "\n" (reverse prog)

runC :: [FourProgram] -> String
runC four = (evalState (compile four) initCS)     

