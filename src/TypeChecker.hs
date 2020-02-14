module TypeChecker where
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as Map
import Control.Monad.Identity 
import AbsLatte
import Errors as Error
import Data.List
import Data.Discrimination (nub, Grouping)
-----------------------Errors---------------------------
showError :: String -> String -> Check String
showError id errorVal = do
                      low <- asks (\env -> context env)
		      return $ concat [ id
                                      , ": "
                                      , errorVal 
                                      , "\nIn: " 
                                      ,  (intercalate "\n    " low)]


showErrorV :: Ident -> String -> Check String
showErrorV (Ident id) errorVal = showError id errorVal

showErrorT :: Type -> Type -> String -> Check String
showErrorT goodType badType str = do
                 low <- asks (\env -> context env)
                 return $ concat [str 
                                 , " Should be " 
                                 , (show goodType) 
                                 , " but actual is " 
                                 , (show badType) 
                                 ,"\n In:" 
                                 , (intercalate "\n    " low)]


------------------------Environment--------------------
type Check = ReaderT CheckEnv (ExceptT String Identity)

initFuns = [((Ident "printInt"), (Void, [Int]))
           , ((Ident "printString"), (Void, [Str]))
           , ((Ident "error"), (Void, [Str]))
           , ((Ident "readInt"), (Int, []))
           , ((Ident "readString"), (Str, []))
           ]

data CheckEnv = CheckEnv { funEnv :: Map.Map Ident (Type, [Type])
                         , varEnv :: Map.Map Ident Type
                         , outVar :: Map.Map Ident Type
                         , retType :: Type
                         , isReturn :: Bool
                         , context :: [String]
                         }

initCheckEnv = CheckEnv { funEnv = Map.fromList initFuns
                        , varEnv = Map.empty
                        , outVar = Map.empty
                        , retType = Void
                        , isReturn = False
                        , context = []
                        }
-----------------------Eval Basic Exp-------------------
data Val = StrVal String | BoolVal Bool | NumVal Integer deriving(Eq)
evalExp :: Expr -> Check (Maybe Val)
evalExp (EString str)   = return $ Just $ StrVal str
evalExp ELitFalse       = return $ Just $ BoolVal False
evalExp ELitTrue        = return $ Just $ BoolVal True
evalExp (ELitInt l)     = return $ Just $ NumVal l

evalExp (EApp (Ident iden) _) = return Nothing
evalExp (EAdd exp1 Minus exp2) = evalTwoExp exp1 exp2 >>= onlyNumAr (-) "substract"
evalExp (EMul exp1 Times exp2) = evalTwoExp exp1 exp2 >>= onlyNumAr (*) "multiply"
evalExp (EMul exp1 Mod exp2)   = evalTwoExp exp1 exp2 >>= onlyNumAr (mod) "modulo"

evalExp (EAdd exp1 Plus exp2) = do
  val <- evalTwoExp exp1 exp2
  case val of
    (Just (NumVal n1, NumVal n2)) -> return $ Just $ NumVal $ n1 + n2
    (Just (StrVal s1, StrVal s2)) -> return $ Just $ StrVal $ s1 ++ s2
    _                      -> return Nothing
evalExp (EMul exp1 Div exp2) = do
  val <- evalTwoExp exp1 exp2
  case val of
    (Just (NumVal n1, NumVal 0))  -> throwError "Cannot divide by zero!"
    (Just (NumVal n1, NumVal n2)) -> return $ Just $ NumVal $ n1 `div` n2
    _                             -> return Nothing
evalExp (ERel exp1 r exp2) = do
  val1 <- evalExp exp1
  val2 <- evalExp exp2
  let tmpFun = (onlyBoolAr (val1, val2))
  case r of
    LTH -> tmpFun "<"  (<)
    LE  -> tmpFun "<=" (<=)
    GTH -> tmpFun ">"  (>)
    GE  -> tmpFun ">=" (>=)
    EQU -> return $ Just $ BoolVal $ val1 == val2
    NE  -> return $ Just $ BoolVal $ val1 /= val2

evalExp (EAnd exp1 exp2) = evalBool (&&) "&&" exp1 exp2
evalExp (EOr exp1 exp2)  = evalBool (||) "||" exp1 exp2
evalExp _                = return Nothing

evalBool :: (Bool -> Bool -> Bool) -> String -> Expr -> Expr -> Check (Maybe Val)
evalBool op name exp1 exp2 = do
  val1 <- evalExp exp1
  val2 <- evalExp exp2
  case (val1, val2) of
    ((Just (BoolVal n1), Just (BoolVal n2))) -> return $ Just (BoolVal (op n1 n2))
    _                                        -> return Nothing

evalTwoExp :: Expr -> Expr -> Check (Maybe (Val, Val))
evalTwoExp exp1 exp2 = do
  val1_ <- evalExp exp1
  val2_ <- evalExp exp2
  case (val1_, val2_) of
    (Just val1, Just val2) -> return $ Just (val1, val2) 
    _                      -> return Nothing

onlyBoolAr :: (Maybe Val, Maybe Val) -> String -> (Integer -> Integer -> Bool) -> Check (Maybe Val)
onlyBoolAr (Just (NumVal n1), Just (NumVal n2)) name op = return $ Just $ BoolVal (op n1 n2)
onlyBoolAr _ _ _ = return Nothing

onlyNumAr :: (Integer -> Integer -> Integer) -> String -> Maybe (Val, Val) -> Check (Maybe Val)
onlyNumAr _ _ Nothing = return Nothing
onlyNumAr op name (Just (NumVal n1, NumVal n2)) = return $ Just $ NumVal (op n1 n2)

-----------------------Ancillary functions---------------
lookupVar :: Ident -> Check Type
lookupVar iden = do
  ttype <- asks (\env -> Map.lookup iden (varEnv env))
  case ttype of  
    Nothing     -> do type2 <- asks (\env ->Map.lookup iden (outVar env))
                      case type2 of
                       Nothing    -> showErrorV iden Error.noVariable >>= throwError
                       (Just l)   -> return l
    (Just l)    -> return l

lookupFun :: Ident -> Check (Type, [Type])
lookupFun iden = do
  ret <- asks (\env -> Map.lookup iden (funEnv env))
  case ret of
    Nothing      -> showErrorV iden Error.noMethod >>= throwError
    (Just types) -> return types

checkIndex :: Expr -> Ident -> Check ()
checkIndex expr iden = do
  ttype <- checkExpr expr
  if ttype == Int then return ()
  else showErrorV iden Error.index >>= throwError

checkAss :: Expr -> Type -> Ident -> Check ()
checkAss expr ttypeV iden =  do
  ttypeE <- checkExpr expr  
  if ttypeE == ttypeV then return ()
  else showErrorV iden Error.assignement >>= throwError

checkInt :: Expr -> String -> Check ()
checkInt expr iden = do
  ttype <- checkExpr expr
  if ttype == Int then return ()
  else showError iden Error.int >>= throwError

checkBool :: Expr -> String -> Check () 
checkBool expr error = do
  ttypeB <- checkExpr expr
  if ttypeB == Bool then return ()
  else showErrorT Bool ttypeB error >>= throwError 

fstDiffer :: [Type] -> [Type] -> (Type, Type)
fstDiffer fst snd = head $ concat $ zipWith (\x y -> [(x, y) | x /= y]) fst snd

changeContext :: ContextStatement -> Check CheckEnv
changeContext newLine = do
                        env <- changeVarDist
                        return env {context = ((show newLine):(context env))} 

changeVarDist :: Check CheckEnv
changeVarDist = do 
  env <- ask
  return env {outVar = Map.union (varEnv env) (outVar env), varEnv = Map.empty}

allDifferent :: Grouping a => [a] -> Bool
allDifferent l = length (Data.Discrimination.nub l) == length l 
 
--------------------Expressions-------------------------
checkExpr :: Expr -> Check Type
checkExpr (ELitInt _)           = return Int
checkExpr ELitTrue              = return Bool
checkExpr ELitFalse             = return Bool
checkExpr (EString _)           = return Str
checkExpr (EVar iden)           = lookupVar iden >>= return
--checkExpr (EArr iden exp)       = checkIndex exp iden >> lookupVar iden >>= return
checkExpr (Neg expr)            = checkInt expr "negation" >> return Int
checkExpr (Not expr)            = checkBool expr Error.wrongCondition >> return Bool
checkExpr (EAdd exp1 Plus exp2) = do
  typeF <- checkExpr exp1
  typeS <- checkExpr exp2
  if typeF == Int && typeS == Int then return Int
  else if typeF == Str && typeS == Str then return Str
       else showError "add" Error.add >>= throwError 

checkExpr (EAdd exp1 _ exp2) = checkExpr (EMul exp1 Times exp2)
checkExpr (EMul exp1 _ exp2) = 
  checkInt exp1 "arithmetic" >> checkInt exp2 "arithmetic" >> return Int

checkExpr (ERel exp1 EQU exp2) = checkExpr (ERel exp1 NE exp2)
checkExpr (ERel exp1 NE exp2) = do 
  type1 <- checkExpr exp1
  type2 <- checkExpr exp2
  if type1 /= type2 then showErrorT type1 type2 Error.comparison >>= throwError
  else return Bool

checkExpr (ERel exp1 rel exp2) =  
   checkInt exp1 (show rel) >> checkInt exp2 (show rel) >> return Bool

checkExpr (EAnd exp1 exp2) = 
  checkBool exp1 (Error.and) >> checkBool exp2 (Error.and) >> return Bool

checkExpr (EOr exp1 exp2) = 
  checkBool exp1 (Error.or) >> checkBool exp2 (Error.or) >> return Bool

checkExpr (EApp iden exps) = do 
  ttypes <- forM exps checkExpr
  (rType, aTypes) <- lookupFun iden 
  if ttypes == aTypes then do return rType
  else do
    if length ttypes /= length aTypes then do (showErrorV iden Error.argumentsNumber) >>= throwError
    else do 
      let (wrongT, expT) = fstDiffer ttypes aTypes 
      showErrorT expT wrongT Error.functionArgument >>= throwError 
 

--------------------Statements--------------------------
checkStmt :: Stmt -> Check CheckEnv 
checkStmt Empty             = ask >>= return 
checkStmt (Decl _ [])       = ask >>= return
checkStmt (SExp expr)       = checkExpr expr >> ask >>= return
checkStmt (While expr stmt) = do
                             val <- evalExp expr
                             newEnv <- changeContext (BrStmt (While expr stmt))  
                             ifEnv <- local (\_ -> newEnv) (checkStmt_ (Cond expr stmt))
                             case val of 
                               (Just (BoolVal True)) -> ask >>= return .
                                                        (\env -> env {isReturn = (isReturn ifEnv)})
                               _                     -> ask >>= return

checkStmt (Decr iden)       = checkStmt (Incr iden)
checkStmt (Cond expr stmt)  = do
                             val <- evalExp expr
                             newEnv <- changeContext (BrStmt (Cond expr stmt)) 
                             ifEnv <- local (\_ -> newEnv) (checkStmt_ (Cond expr stmt))
                             case val of
                               (Just (BoolVal True)) -> ask >>= return . 
                                                       (\env -> env {isReturn = (isReturn ifEnv)})
                               _ -> ask >>= return

checkStmt (CondElse expr stmt1 stmt2) = do 
                             val <- evalExp expr
                             newEnv <- changeContext (BrStmt (Cond expr stmt1))
                             ifEnv <- local (\_ -> newEnv) (checkBool expr Error.wrongCondition >> checkStmt_ (Cond expr stmt1))
                             newEnv2 <- changeContext (BrStmt (CondElse expr stmt1 stmt2))
                             elseEnv <- local (\_ -> newEnv2) (checkStmt_ (Cond ELitTrue stmt2))
                             case val of
                               Nothing   -> ask >>= return . 
                                        (\env -> env {isReturn = (isReturn ifEnv) && (isReturn elseEnv)})
                               (Just (BoolVal True)) -> ask >>= return . 
                                        (\env -> env {isReturn = isReturn ifEnv})
                               (Just (BoolVal False)) -> ask >>= return .
                                        (\env -> env {isReturn = isReturn elseEnv})

checkStmt (BStmt (Blk block)) = do
  env <- changeContext (BrStmt (BStmt (Blk block)))
  (foldM (\env stmt -> local (\_ -> env) (checkStmt stmt)) env block) >>= return  

checkStmt VRet = do
  ttype <- asks (\env -> retType env)
  if ttype == Void then ask >>= return
  else showErrorT Void ttype Error.wrongReturn >>= throwError

checkStmt (Incr iden) = do
  ttype <- lookupVar iden
  if ttype == Int then ask >>= return 
  else showErrorV iden Error.int >>= throwError

checkStmt (Ass iden exp) = do
  ttype <- lookupVar iden
  etype <- checkExpr exp
  if ttype == etype then ask >>= return
  else showErrorT ttype etype Error.assignement >>= throwError

----
--checkStmt (ArAss iden exprInd exprVal) = do
--  ttype <- lookupVar iden
--  checkIndex exprInd iden
--  checkAss exprVal ttype iden 
--  ask >>= return 

checkStmt (Ret expr) = do
  ttype  <- checkExpr expr
  ttypeR <- asks (\env -> retType env)
  if ttypeR == ttype then ask >>= return . (\env -> env {isReturn = True})
  else showErrorT ttypeR ttype Error.wrongReturn >>= throwError

checkStmt (Decl ttype ((Init iden expr):k)) = do
  checkAss expr ttype iden
  checkStmt (Decl ttype ((NoInit iden):k))

checkStmt (Decl ttype ((NoInit iden):k)) = do
  env <- ask
  let el = Map.lookup iden (varEnv env)
  case el of
    Nothing -> local (\_ ->  env {varEnv = Map.insert iden ttype (varEnv env)}) 
                                                       (checkStmt (Decl ttype k))
    (Just _) -> showErrorV iden Error.declared >>= throwError
--    (Array iden expr) -> checkIndex expr iden >> checkStmt (Decl ttype ((NoInit iden):k)) 
--    (ArrayInit iden []) -> checkStmt (Decl ttype ((NoInit iden):k))
--    (ArrayInit iden (expr:t)) -> 
--             checkAss expr ttype iden >> checkStmt (Decl ttype ((ArrayInit iden t):k))

checkStmt_ (Cond expr stmt)  =  
  checkBool expr Error.wrongCondition >> checkStmt stmt 

--------------------Top Definitions-------------------------
putArgs :: TopDef -> Check CheckEnv
putArgs def = do
    env <- ask
    let names = [iden | (Ar x (Ident iden)) <- args def]
    if (allDifferent names) then do 
      let vars = map (\(Ar x iden) -> (iden, x)) (args def) 
      return $ env {varEnv =  (Map.fromList vars)}
    else showError (show (FunDec def)) Error.declaredParameter >>= throwError

checkFun :: TopDef -> Check () 
checkFun def = do
  aEnv <- putArgs def 
  let pEnv = aEnv {retType = tye def, context = [show (FunDec def)]}
  let (Blk b) = (block def)  
  env <- foldM (\env stmt -> local (\_ -> env) (checkStmt stmt)) pEnv b
  if isReturn env || retType env == Void then return ()
  else showError (show (FunDec def)) Error.noReturn >>= throwError 

argsTypes :: [Arg] -> [Type]
argsTypes args = map (\(Ar type_ _) -> type_) args

addAllTopDef :: [TopDef] -> Check CheckEnv 
addAllTopDef [] = ask >>= return
addAllTopDef ((FnDef ftype iden args _):k) = do
  env <- ask
  local (\_ -> env {funEnv = Map.insert iden (ftype, (argsTypes args)) (funEnv env)})                        (addAllTopDef k)


--------------------Program--------------------------------
checkProgram :: Program -> Check () 
checkProgram (Prog l) = do
  env <- addAllTopDef l
  forM_ l (\def -> local (\_ -> env) (checkFun def))
  return ()

runCheck p = runIdentity (runExceptT (runReaderT (checkProgram p) initCheckEnv))
