module Interpreter where

import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.State
import Data.Map as Map
import qualified Data.IntMap.Strict as IntMap
import Data.Functor.Identity
import AbsLatte
import Control.Monad.Except
import Data.Array
import Data.List.Lens
import Control.Lens hiding(Empty)
import Control.Lens.At
import Data.AList as AList
import Errors as Error

type Interpreter a = ReaderT Env (StateT Store (ExceptT String IO)) a

-------------------------HELPER FUNCTIONS----------------------------
showError :: String -> String -> String
showError iden error = (iden ++ ": " ++ error)

lookupTopDef :: String -> Interpreter TopDef
lookupTopDef name = do
  def <- asks (\env -> Map.lookup (Ident name) (fEnv env)) 
  case def of 
    Nothing    -> throwError (name ++ ": " ++ Error.noMethod)
    (Just def) -> return def

lookupVar :: Ident -> Interpreter Loc
lookupVar iden = do
  loc   <- asks (\env -> Map.lookup iden (vEnv env))
  case loc of  
    Nothing     -> throwError ((show iden) ++ ": " ++ Error.noVariable)
    (Just l)    -> return l

lookupStore :: Loc -> Interpreter (Loc, Val)
lookupStore loc = do
  store <- get
  case IntMap.lookup loc store of
    Nothing      -> throwError (Error.memoryCorrupt)
    Just val     -> return (loc, val)

putItem :: Type -> Item -> Interpreter Env
putItem ttype (NoInit iden)    = putItem ttype (Init iden (initVal ttype)) 
putItem ttype (Array iden exp) = do
  evalExp exp >>= (\(NumVal siz) -> putItem ttype 
                                    (ArrayInit iden (replicate (fromIntegral siz) (initVal ttype))))

putItem ttype (Init iden exp) = do
  nextloc     <- gets IntMap.size 
  evalExp exp >>= (\v -> modify (IntMap.insert nextloc v))
  ask         >>= (\env -> return env {vEnv = Map.insert iden nextloc (vEnv env)}) 

putItem ttype (ArrayInit iden exps) = do
  nextloc           <- gets IntMap.size
  mapM evalExp exps >>= (\list -> modify (IntMap.insert nextloc (ArrayVal (list))))
  ask               >>= (\env -> return env {vEnv = Map.insert iden nextloc (vEnv env)}) 

putArgToEnv :: (Arg, Expr) -> Interpreter Env
putArgToEnv ((), exp) = do
  env <- ask
  case arg of
    (Arg (Point _) iden) -> case exp of 
                       (EVar idenV) -> lookupVar idenV >>=  
                                        (\loc -> return env {vEnv = Map.insert iden loc (vEnv env)}) 
                       _            -> throwError "pointer to non-variable" 
    (Arg _ iden) -> do 
                   nextloc <- gets IntMap.size
                   val <- evalExp exp
                   modify (IntMap.insert nextloc val) 
                   return env {vEnv = Map.insert iden nextloc (vEnv env)}


---------------EXPR--EVALUATION---------------------
evalExp :: Expr -> Interpreter Val
evalExp (EString str)   = return (StrVal str)
evalExp ELitFalse       = return (BoolVal False)
evalExp ELitTrue        = return (BoolVal True)
evalExp (ELitInt l)     = return (NumVal l)
evalExp (EVar ind)      = lookupVar ind >>= lookupStore >>= (\(_, val) -> return val)
evalExp (EApp (Ident "error") [exp])   = evalExp exp >>= throwError . show  
evalExp (EApp (Ident "printStr") exps)  = evalExp (EApp (Ident "printInt") exps)
evalExp (EApp (Ident "printInt") [exp]) = 
  evalExp exp >>= (liftIO . putStr . show)  >> return VoidVal

evalExp (EArr iden exp) = 
  evalExp exp >>= (\(NumVal e) -> lookupVar iden >>= 
              lookupStore >>= (\(_, (ArrayVal ar)) -> return (ar !! (fromIntegral e))))

evalExp (EApp (Ident iden) [])  = do
  (FnDef ttype id args block) <- lookupTopDef iden
  env <- ask
  local (\_ -> env) (execStmt (BStmt block)) >>= return . retVal

evalExp (EApp (Ident iden) l) = do
  env <- ask
  (FnDef ttype id args block) <- lookupTopDef iden
  callEnv <- foldM (\ev arg -> local (\_ -> ev) (putArgToEnv arg)) env (zip args l)
  resEnv  <- local (\_ -> callEnv) (execStmt (BStmt block)) 
  if ttype /= Void && (retVal resEnv) == NullVal then throwError (showError iden Error.noReturn)
  else return (retVal resEnv)


-------------------ARITHMETIC------------------------ 
evalExp (EAdd exp1 Minus exp2) = evalTwoExp exp1 exp2 >>= onlyNumAr (-) "substract" 
evalExp (EMul exp1 Times exp2) = evalTwoExp exp1 exp2 >>= onlyNumAr (*) "multiply"
evalExp (EMul exp1 Mod exp2)   = evalTwoExp exp1 exp2 >>= onlyNumAr (mod) "modulo"

evalExp (EAdd exp1 Plus exp2) = do 
  val <- evalTwoExp exp1 exp2
  case val of
    (NumVal n1, NumVal n2) -> return $ NumVal $ n1 + n2
    (StrVal s1, StrVal s2) -> return $ StrVal $ s1 ++ s2 
    (val1, val2)           -> throwError ((show val1) ++ " cannot be added to " ++ (show val2))

evalExp (EMul exp1 Div exp2) = do
  val <- evalTwoExp exp1 exp2
  case val of
    (NumVal n1, NumVal 0)  -> throwError "Cannot divide by zero!"
    (NumVal n1, NumVal n2) -> return $ NumVal $ n1 `div` n2
    (val1, val2)           -> throwError ((show val1) ++ " cannot be divided by " ++ (show val2))

evalExp (ERel exp1 r exp2) = do
  val1 <- evalExp exp1
  val2 <- evalExp exp2
  let tmpFun = (onlyBoolAr (val1, val2))
  case r of
    LTH -> tmpFun "<"  (<)
    LE  -> tmpFun "<=" (<=)
    GTH -> tmpFun ">"  (>)
    GE  -> tmpFun ">=" (>=)
    EQU -> return $ BoolVal $ val1 == val2
    NE  -> return $ BoolVal $ val1 /= val2

evalExp (EAnd exp1 exp2) = evalBool (&&) "&&" exp1 exp2
evalExp (EOr exp1 exp2)  = evalBool (||) "||" exp1 exp2
 
evalBool :: (Bool -> Bool -> Bool) -> String -> Expr -> Expr -> Interpreter Val
evalBool op name exp1 exp2 = do
  val1 <- evalExp exp1
  val2 <- evalExp exp2
  case (val1, val2) of
    (BoolVal n1, BoolVal n2) -> return (BoolVal (op n1 n2))
    _                        -> throwError (Error.bool++ " " ++ name ++ ".")

evalTwoExp :: Expr -> Expr -> Interpreter (Val, Val)
evalTwoExp exp1 exp2 = do
  val1 <- evalExp exp1
  val2 <- evalExp exp2
  return (val1, val2) 

onlyBoolAr :: (Val, Val) -> String -> (Integer -> Integer -> Bool) -> Interpreter Val
onlyBoolAr (NumVal n1, NumVal n2) name op = return $ BoolVal (op n1 n2)

onlyNumAr :: (Integer -> Integer -> Integer) -> String -> (Val, Val) -> Interpreter Val
onlyNumAr op name (NumVal n1, NumVal n2) = return $ NumVal (op n1 n2)


--------------STMT--EXECUTION------------------------
execStmt :: Stmt -> Interpreter Env
execStmt Empty              = ask >>= return
execStmt (While expr stmt)  = do
  val <- evalExp expr
  case val of
    (BoolVal True) -> execStmt stmt >>= 
                            (\env -> if bbreak env then 
                                          return env {bbreak = False, ccontinue = False} 
                                     else (local (\_ -> env {ccontinue = False}) 
                                          (execStmt (While expr stmt))))
    (BoolVal False)-> ask >>= return

execStmt (BStmt (Block (h:t))) = do
  env <- execStmt h
  case (retVal env) of
     NullVal     ->  local (\_ -> env) (execStmt (BStmt (Block t)))
     _           ->  return env 

execStmt op = do
  stop <- asks (\env -> (bbreak env) || (ccontinue env))
  if stop then ask >>= return
  else case op of
    VRet               -> ask >>= (\env -> return env {retVal = VoidVal}) 
    (BStmt (Block [])) -> ask >>= return
    Break              -> ask >>= (\env -> return env {bbreak = True})
    Contin             -> ask >>= (\env -> return env {ccontinue = True})
    (SExp expr)        -> evalExp expr >> ask >>= return
    (Cond expr stm)    -> execStmt (CondElse expr stm Empty)
    (ArAss iden exp1 exp2) -> do
      (NumVal index) <- evalExp exp1
      val            <- evalExp exp2  
      lookupVar iden >>= lookupStore >>= 
             (\(loc, (ArrayVal arr)) -> modify -- below array value change 
                          (IntMap.insert loc (ArrayVal (arr & ix (fromIntegral index) .~ val))))
      ask >>= return  
    (Ret exp)      -> do
       val <- evalExp exp  
       ask >>= (\env -> return env {retVal = val})
    (Ass iden exp) -> do
       val <- evalExp exp
       lookupVar iden >>= lookupStore >>= (\(loc, _) -> modify (IntMap.insert loc val))
       ask >>= return
    (Decl ttype (x:l)) -> do
       env <- putItem ttype x 
       local (\_ -> env) (execStmt (Decl ttype l))  
    (Decl ttype []) -> ask >>= return
    (CondElse expr stmt stmf) -> do
      val <- evalExp expr
      case val of 
        (BoolVal True) -> execStmt stmt
        (BoolVal False)-> execStmt stmf
    (Incr iden) -> do
      lookupVar iden >>= lookupStore >>= 
            (\(loc, (NumVal v)) -> modify (IntMap.insert loc (NumVal (v+1)))) 
      ask >>= return  
    (Decr iden) -> do  
      lookupVar iden >>= lookupStore >>= 
            (\(loc, (NumVal v)) -> modify (IntMap.insert loc (NumVal (v)))) 
      ask >>= return  


---------------------EXECUTION-------------------------
execProgram :: Program -> Interpreter Env
execProgram (Program (t:h)) = do
  env <- execTopDef t
  local (\_ -> env) (execProgram (Program h))

execProgram (Program []) = do
  env  <- ask
  def  <- lookupTopDef "main"
  case def of  
    (FnDef int (Ident "main") (_:l) _) -> 
                throwError ("main takes no arguments (" ++ (show ((length l) + 1)) ++ " given)")
    (FnDef int (Ident "main") [] _)    -> 
                local (\_ -> env) (evalExp (EApp (Ident "main") [])) >> return env
    _          -> throwError "syntax error in main"
    
runProgram p = runExceptT (runStateT (runReaderT (execProgram p) initEnv) initStore)

execTopDef :: TopDef -> Interpreter Env
execTopDef (GbDef ttype iden) = putItem ttype (NoInit iden)

execTopDef (FnDef ttype iden args b) = 
  ask >>= (\env -> return env {fEnv = Map.insert iden (FnDef ttype iden args b) (fEnv env)})
