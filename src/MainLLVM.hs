module Main where

import System.Environment (getArgs)
import LLVMBackend ( compileLLVM, postCompilationLLVM )
import Utils ( usage, compile )
import Data.List (delete)

main :: IO ()
main = do
  args <- getArgs
  let noPostCompile = "-c" `elem` args
  let args_no_flags = delete "-c" args
  case args_no_flags of
    ["--help"] -> usage
    [filepath] -> compile filepath ".ll" compileLLVM postCompilationLLVM noPostCompile
    _ -> usage