module Utils where

import AbsInstant ( Program )
import ParInstant (pProgram, myLexer)
import LexInstant (mkPosToken)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import PrintInstant (printTree)
import Control.Monad.Except (ExceptT, runExceptT)
import System.FilePath (replaceExtension)

endlStr, emptyStr :: ShowS
emptyStr = showString ""
endlStr = showString "\n"

formatLines :: [String] -> ShowS
formatLines = foldr (\x acc -> showString x . endlStr . acc) emptyStr

usage :: IO ()
usage = do
  putStrLn $ unlines [
        "Usage:  Call with one of the following argument combinations:"
        , "  --help          Display this help message."
        , "  file            Compile content of the file."
        , "  -c file         Compile content of the file. Do not generate LLVM bitcode/Java class."
        ]
  exitFailure

parse :: String -> IO Program
parse s = do
  case pProgram (myLexer s) of
    Left err -> do
      putStrLn "Parse error"
      putStrLn "\nParse              Failed...\n"
      putStrLn "Tokens:"
      mapM_ (putStrLn . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      return tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]


compile :: FilePath -> String -> (Program -> ExceptT String IO String) -> (String -> IO ()) -> Bool -> IO ()
compile filepath format compiler postCompilationAction noPostCompile = do
  s <- readFile filepath
  parsed_program <- parse s
  compilationResult <- runExceptT (compiler parsed_program)

  case compilationResult of
    Left e -> do
      hPutStrLn stderr $ "[Compilation error] " ++ e
      exitFailure
    Right compiledProgram -> do
      let compiled_filepath = replaceExtension filepath format
      writeFile compiled_filepath compiledProgram 
      putStrLn (showString "Generated: " compiled_filepath)
      if noPostCompile 
        then exitSuccess 
        else do
          postCompilationAction compiled_filepath
          exitSuccess 


