module LLVMBackend
  ( compileLLVM,
    postCompilationLLVM ) where

import AbsInstant ( Program (Prog), Stmt (SExp, SAss), Exp (ExpAdd, ExpSub, ExpDiv, ExpMul, ExpLit, ExpVar), Ident )
import Data.Map (Map, lookup, empty, insert)
import Control.Monad.State
    ( foldM, modify, MonadState(get), MonadTrans(lift), StateT, execStateT, gets, evalStateT )
import Control.Monad.Except ( MonadError(throwError), ExceptT )

import qualified Control.Arrow as Data.Bifunctor
import Utils (formatLines, endlStr, emptyStr)
import System.FilePath ( replaceExtension )
import System.Process (callCommand)

type Context = (Int, Map Ident Register)

type Compiler m = (StateT Context (ExceptT String IO)) m

type Register = ShowS

data Operation =
  OArithmetic {
    resultReg :: Register,
    instruction :: ShowS,
    arg1 :: Number,
    arg2 :: Number
  }
  | OLoad {
    resultReg :: Register,
    loc :: Register
  }
  | OStore {
    val  :: Number,
    loc :: Register
  }
  | OAlloc {
    loc  :: Register
  }
  | OCall {
    fname :: ShowS,
    arg :: Number
  }

data Number = Const Integer | Variable Register

loadVar :: Ident -> Compiler ([Operation], Number)
loadVar ident = do
  context <- get
  let (_, variables) = context
  case Data.Map.lookup ident variables of
    Just locationRegister -> do
      loadRegister <- getRegister
      let loadOp = OLoad loadRegister locationRegister
      return ([loadOp], Variable loadRegister)
    Nothing -> throwError $ "Using undeclared variable: " ++ show ident

storeVar :: Ident -> Number -> Compiler [Operation]
storeVar ident value = do
  context <- get
  let (_, variables) = context
  case Data.Map.lookup ident variables of
    Just locationRegister -> do
      let storeOp = OStore value locationRegister
      return [storeOp]
    Nothing -> do
      locationRegister <- getRegister
      let allocOp = OAlloc locationRegister
      let storeOp = OStore value locationRegister
      modify (Data.Bifunctor.second (insert ident locationRegister))
      return [storeOp, allocOp]

getRegister :: Compiler Register
getRegister = do
  modify (\(registerId, varMap) -> (registerId + 1, varMap))
  counter <- gets fst
  let s = showString "%r" . showString (show counter)
  return s

printInt :: Number -> Compiler [Operation]
printInt number = return [OCall (showString "printInt") number]

expandArithmeticOp :: Exp -> Exp -> ShowS -> Compiler ([Operation], Number)
expandArithmeticOp exp1 exp2 op = do
  (op1, r1) <- translateExp exp1
  (op2, r2) <- translateExp exp2
  result_register <- getRegister
  let new_op = OArithmetic result_register op r1 r2
  let operations = new_op : (op2 ++ op1)
  return (operations, Variable result_register)

translateExp :: Exp -> Compiler ([Operation], Number)
translateExp expr = do
  case expr of
    ExpAdd exp1 exp2 -> expandArithmeticOp exp1 exp2 (showString "add")
    ExpSub exp1 exp2 -> expandArithmeticOp exp1 exp2 (showString "sub")
    ExpMul exp1 exp2 -> expandArithmeticOp exp1 exp2 (showString "mul")
    ExpDiv exp1 exp2 -> expandArithmeticOp exp1 exp2 (showString "udiv")
    ExpLit number ->
      if number < 0
        then
          throwError $ "Negative values are forbidden: " ++ show number
        else return ([], Const number)
    ExpVar ident -> loadVar ident

translateStmt :: Stmt -> Compiler [Operation]
translateStmt (SExp exp) = do
  (expOperations, result) <- translateExp exp
  printOperations <- printInt result
  return $ printOperations ++ expOperations

translateStmt (SAss ident exp) = do
  (expOperations, result) <- translateExp exp
  storeOperations <- storeVar ident result
  return $ storeOperations ++ expOperations

llvmIntro, llvmOutro :: ShowS
llvmIntro = formatLines
  [ "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\""
  , "declare i32 @printf(i8*, ...)"
  , ""
  , "define void @printInt(i32 %x) {"
  , "  %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0"
  , "  call i32 (i8*, ...) @printf(i8* %t0, i32 %x)"
  , "  ret void"
  , "}"
  , ""
  , "define i32 @main() {"
  ]
llvmOutro = formatLines ["  ret i32 0", "}"]

translateProgram :: Program -> Compiler [Operation]
translateProgram (Prog stmts) = foldM mop [] stmts
  where
    mop :: [Operation] -> Stmt -> Compiler [Operation]
    mop ops stmt = do
      new_ops <- translateStmt stmt
      let all_ops = new_ops ++ ops
      return all_ops

showNumber :: Number -> ShowS
showNumber  x =
  case x of
    Const n -> showString (show n)
    Variable g -> g

translateOperation :: Operation -> ShowS
translateOperation operation = case operation of
  OArithmetic resultReg operation num1 num2 ->
    showString "  "
    . resultReg
    . showString " = "
    . operation
    . showString " i32 "
    . showNumber num1
    . showString ", "
    . showNumber num2
    . endlStr
  OLoad resultReg loc ->
    showString "  "
    . resultReg
    . showString " = load i32, i32* "
    . loc
    . endlStr
  OStore val register ->
    showString "  store i32 "
    . showNumber val
    . showString ", i32* "
    . register
    . endlStr
  OAlloc loc ->
    showString "  "
    . loc
    . showString " = "
    . showString "  alloca i32"
    . endlStr
  OCall fname arg ->
    showString "  call void @"
    . fname
    . showString "(i32 "
    . showNumber arg
    . showString ")"
    . endlStr

showArgs :: [Number] -> ShowS
showArgs = foldr (\number acc -> showString "i32 " . showNumber number . acc) emptyStr

showOps :: [Operation] -> ShowS
showOps = foldr (\operation acc -> acc . translateOperation operation) emptyStr

callLLVM :: String -> String -> IO ()
callLLVM filepath llvmBitcode_path = callCommand $ (showString "llvm-as -o " . showString llvmBitcode_path . showString " ") filepath

compileLLVM :: Program -> ExceptT String IO String
compileLLVM p = do
  let empty_context = (0, Data.Map.empty)
  operations <- evalStateT (translateProgram p) empty_context
  let str_operations = foldr (\operation acc -> acc . translateOperation operation) emptyStr operations
  let compiledProgram = llvmIntro . str_operations . llvmOutro $ ""
  return compiledProgram

postCompilationLLVM :: String -> IO ()
postCompilationLLVM filepath = do
  let llvmBitcode_path = replaceExtension filepath ".bc"
  callLLVM filepath llvmBitcode_path
  putStrLn (showString "Generated: " llvmBitcode_path)
