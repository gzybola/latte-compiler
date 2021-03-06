module StringGen where

import Data.Map as M
import Control.Monad.Writer
import Control.Monad.State
import Data.Hex
import Absfvc
import Data.List
import Numeric (showHex, showIntAtBase)

checkAtom :: Atom -> StringChecker ()
checkAtom atom = do
  case atom of
    (AtomStr str) -> tell $ Endo ([str]<>)
    _ -> return ()

type StringChecker = Writer (Endo [String])

findSStmt :: NonJmpStmt -> StringChecker ()
findSStmt (StmtBinOp _ atom1 _ atom2) = checkAtom atom1 >> checkAtom atom2
findSStmt (StmtNegOp _ atom) = checkAtom atom
findSStmt (StmtCall _ atoms) = mapM_ checkAtom atoms
findSStmt (StmtCallRet _ _ atoms) = mapM_ checkAtom atoms 
findSStmt (StmtEmpty) = return ()
findSStmt (StmtNoOp _ aaatom) = checkAtom aaatom 

findSJS :: JmpStmt -> StringChecker ()
findSJS (StmtRet atom) = checkAtom atom
findSJS _              = return ()

findStr :: CodeFlow -> StringChecker ()
findStr (Flow (CodeBlock _ stmts jmp) flows)= do
  mapM_ findSStmt stmts
  findSJS jmp
  mapM_ findStr flows
findStr Leaf = return ()

findFunStr :: FourProgram -> StringChecker ()
findFunStr (FourProg _ _ _ tree) = findStr tree 

findProgStr :: [FourProgram] -> StringChecker ()
findProgStr progs = do
    mapM_ findFunStr progs
    

allStrings :: [FourProgram] -> (String, Map String String)
allStrings prg = (res, mapStr)
    where
        strs = appEndo (execWriter (findProgStr prg)) []
        (res,(mapStr, _)) = runState (manageStr strs) (M.empty, 1) 

strRep :: String -> String
strRep str = "\"" ++ concat (Data.List.map (\s -> ("\\x" ++ (showHex (fromEnum s) ""))) ( str)) ++ "\""

type StringMap = State SEnv

type SEnv = (M.Map String String, Integer)

manageStr :: [String] -> StringMap String
manageStr strs = do
    resStr <- mapM stringData strs
    return $ intercalate "\n" resStr

stringData :: String -> StringMap String
stringData str = do
    (encd, counter) <- get
    let name = "_string_" ++ (show counter)
    put(M.insert str name encd, counter + 1)
    return ("\n.section .rodata\n\
             \.align 8\n\
             \.type " ++ name ++ ", @object\n" ++ name ++ ":\n" ++ "        .long "++(show $ length str)++ "\n.string " ++ (strRep str))
