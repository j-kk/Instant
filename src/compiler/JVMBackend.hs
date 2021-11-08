module JVMBackend
 ( compileJVM,
   postCompilationJVM) where

import Control.Monad.Except (ExceptT, MonadError (throwError), foldM, MonadTrans (lift))

import AbsInstant ( Program (Prog), Stmt (SExp, SAss), Exp (ExpAdd, ExpSub, ExpDiv, ExpMul, ExpLit, ExpVar), Ident )
import Data.Map (Map, lookup, empty, insert)
import Control.Monad.State (StateT, MonadState (get, put), modify, evalStateT)
import qualified Control.Arrow as Data.Bifunctor
import Utils (formatLines, endlStr, emptyStr)
import System.Process ( callCommand )
import System.FilePath ( takeDirectory )
import LexInstant (posLineCol)


type Location = Int
-- locals stack stackMax
type Context = (Int, Int, Int, Map Ident Location)

type Compiler m = (StateT Context (ExceptT String IO)) m

getLocation :: Ident -> Compiler Location
getLocation ident = do
  (locals, stack, stackMax, variables) <- get
  case Data.Map.lookup ident variables of
    Just location -> do
      return location
    Nothing -> do
      put (locals + 1, stack, stackMax, insert ident locals variables)
      return locals

updateStackCounter :: Int -> Compiler ()
updateStackCounter n = do
  (locals, stack, stackMax, variables) <- get
  let stack' = stack + n
  let stackMax' = max stackMax stack'
  put (locals, stack', stackMax', variables)

translateExp :: Exp -> Compiler ShowS
translateExp exp = case exp of
  ExpAdd exp1 exp2 -> expandArithmeticOp' exp1 exp2 (showString "iadd")
  ExpSub exp1 exp2 -> expandArithmeticOp exp1 exp2 (showString "isub")
  ExpMul exp1 exp2 -> expandArithmeticOp' exp1 exp2 (showString "imul")
  ExpDiv exp1 exp2 -> expandArithmeticOp exp1 exp2 (showString "idiv")
  ExpLit n ->
    if n < 0
      then
        throwError $ "Negative values are forbidden: " ++ show n
      else do
        updateStackCounter 1
        let operation | n == -1                              = showString "  iconst_m1"
                      | n `elem` [0 .. 5]                    = append_n "  iconst_"
                      | n >= -byteRange && n < byteRange     = append_n "  bipush "
                      | n >= -shortRange && n < shortRange   = append_n "  sipush "
                      | otherwise                            = append_n "  ldc "
        return $ operation . endlStr
        where
          byteRange  = 128
          shortRange = 32768
          append_n :: String-> ShowS
          append_n op_string = showString op_string . shows n
  ExpVar ident -> do
    varLocation <- getLocation ident
    updateStackCounter 1
    return $ loadOp varLocation
    where 
      loadOp :: Int -> ShowS
      loadOp index = 
        let op = if index `elem` [0..3] then "  iload_" else "  iload "
        in showString op . shows index . endlStr

expandArithmeticOp :: Exp -> Exp -> ShowS -> Compiler ShowS
expandArithmeticOp exp1 exp2 arithmeticOp = do
  operations1 <- translateExp exp1
  operations2 <- translateExp exp2
  updateStackCounter (-1)
  let new_op = showString "  " . arithmeticOp . endlStr
  return $ operations1 . operations2 . new_op


expandArithmeticOp' :: Exp -> Exp -> ShowS -> Compiler ShowS
expandArithmeticOp' exp1 exp2 arithmeticOp = do
  (locals, stack, maxStack, vars) <- get
  operations1 <- translateExp exp1
  (_, stack1, maxStack1,_) <- get
  put (locals, stack, maxStack, vars)
  operations2 <- translateExp exp2
  (_, stack2, maxStack2,_) <- get
  if maxStack2 < maxStack1
    then do
      put (locals, stack1, maxStack1, vars)
      operations2Cont <- translateExp exp2
      addCurrentOp arithmeticOp (operations1 . operations2Cont)
    else do
      put (locals, stack2, maxStack2, vars)
      operations1Cont <- translateExp exp1
      addCurrentOp arithmeticOp (operations2 . operations1Cont)

  where
    addCurrentOp :: ShowS -> ShowS -> Compiler ShowS
    addCurrentOp arithmeticOp operations = do
      updateStackCounter (-1)
      let new_op = showString "  " . arithmeticOp . endlStr
      return $ operations . new_op

translateStmt :: Stmt -> Compiler ShowS
translateStmt stmt = case stmt of
  SAss ident exp -> do
    operations <- translateExp exp
    updateStackCounter (-1)
    varLocation <- getLocation ident
    return $ operations . storeOp varLocation
  SExp exp -> do
    let load_print = showString "  getstatic  java/lang/System/out Ljava/io/PrintStream;" . endlStr
    updateStackCounter 1
    operations <- translateExp exp
    let call_print = showString "  invokevirtual  java/io/PrintStream/println(I)V" . endlStr
    updateStackCounter (-2)
    return $ load_print . operations . call_print
  
  where 
    storeOp :: Int -> ShowS
    storeOp index = 
      let op = if index `elem` [0..3] then "  istore_" else "  istore "
      in showString op . shows index . endlStr

jvmIntro :: String -> ShowS
jvmIntro filename = formatLines
  [ ".class  public " ++ filename
  , ".super  java/lang/Object"
  , ".method public <init>()V"
  , "  aload_0"
  , "  invokespecial java/lang/Object/<init>()V"
  , "  return"
  , ".end method"
  , ".method public static main([Ljava/lang/String;)V"
  ]

jvmOutro :: ShowS
jvmOutro = formatLines
  [ "  return"
  , ".end method"
  ]

jvmLimits :: Compiler ShowS
jvmLimits = do
  (locals, _, stackMax, _) <- get
  return $ formatLines
    [ ".limit locals " ++ show (locals + 1)
    , ".limit stack " ++ show stackMax
    ]

translateProgram :: String -> Program -> Compiler String
translateProgram filename (Prog stmts) = do
  let prolog = jvmIntro filename
  program <- foldM mop emptyStr stmts
  limits <- jvmLimits
  let epilog = jvmOutro
  return $ (prolog . limits . program . epilog) ""
  where
    mop :: ShowS -> Stmt -> Compiler ShowS
    mop old_ops stmt = do
      new_ops <- translateStmt stmt
      return $ old_ops . new_ops

callJasmin :: String -> IO ()
callJasmin filepath = do
  callCommand $ (showString "java -jar ./lib/jasmin.jar -d " . showString (takeDirectory filepath) . showString " ") filepath

compileJVM :: String -> Program -> ExceptT String IO String
compileJVM filename p = do
  let empty_context = (0,0,0, Data.Map.empty)
  evalStateT (translateProgram filename p) empty_context

postCompilationJVM :: String -> IO ()
postCompilationJVM = callJasmin