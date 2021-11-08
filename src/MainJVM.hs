module Main where

import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import JVMBackend ( compileJVM, postCompilationJVM )
import Utils ( usage, compile )
import Data.List ( delete )


main :: IO ()
main = do
  args <- getArgs
  let noPostCompile = "-c" `elem` args
  let args_no_flags = delete "-c" args
  case args_no_flags of
    ["--help"] -> usage
    [filepath] -> compile filepath ".j" (compileJVM (takeBaseName filepath)) postCompilationJVM noPostCompile
    _ -> usage