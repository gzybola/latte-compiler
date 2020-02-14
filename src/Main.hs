module Main where
import System.Exit (exitFailure, exitSuccess)
import System.IO
import System.Environment (getArgs)
import Control.Monad
import ErrM
import System.Process
import PrintLatte
import FourValue
import AbsLatte
import LexLatte
import ParLatte
import ErrM
import TypeChecker
import ASB
import System.FilePath.Posix ( replaceExtension, takeDirectory, takeBaseName )
import Data.List
import System.Directory (getCurrentDirectory)
type ParseFun a = [Token] -> Err a
type Verbosity = Int

exitWithError :: String -> IO ()
exitWithError message = do
  hPutStr stderr $ "ERROR\n" ++ message
  exitFailure

exitOK :: IO ()
exitOK = hPutStr stderr $ "OK\n"

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()


run p s file = let ts = myLexer s in case p ts of
           ErrM.Bad s  -> do exitWithError s
           Ok  tree    -> do 
                           case runCheck tree of 
                            (Right _)       -> do 
                                               let res = runConvert tree 
                                               let res2 = runC res
                                               let sFile = replaceExtension file "s"
                                               let oFile = takeBaseName file
                                               let dir   = takeDirectory file
                                               myDir <- getCurrentDirectory
                                               writeFile sFile res2
                                               callCommand $ intercalate " " [
                                                 "gcc -m32", 
                                                 sFile, 
                                                 myDir++"/lib/runtime.o", 
                                                 "-o", 
                                                 dir++"/"++oFile ]
                                               exitOK
                            (Left message)  -> exitWithError message

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1 then
    error "Wrong number of arguments"
  else do
    let file = head args
    inputProgram <- readFile file
    run pProgram inputProgram file
