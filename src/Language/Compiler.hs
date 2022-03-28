{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Compiler (compile) where

import Language.Compiler.Combinators
import Language.Compiler.Constants
import Language.Compiler.Types
import Control.Monad.State

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Bits
import Data.Int

import System.Process hiding (env)

import Language.Syntax
import Data.ASM

compile prog = do
  let compC = runCompC $ compileProgram prog
      asmCode = T.unlines $ map formatASM' compC
      output = asmHeader <> asmCode
  T.writeFile "output.s" output
  runGCC

runGCC = do
  readProcessWithExitCode "gcc" (words "-g assets/driver.c output.s --omit-frame-pointer -o main") ""
  pure ()

compileProgram :: Program -> CompC ()
compileProgram (List [Atom "labels", List lvars, body]) = do
  movq rdi rsi
  let mainLabel = "main"
  jmp mainLabel
  mapM_ (\(List [Atom lvar, lexpr]) -> do
            l <- uniqueLabel' lvar
            extendEnv lvar (LabelLocation l)
            label l
            emitLexpr lexpr) lvars
  label mainLabel
  emitExpr body
  ret
compileProgram _ = error "malformed program"

emitLexpr (List [Atom "code", List args, List freeVars, body]) = do
  let argIndices = map (* (-wordsize)) [1..]
      freeVarIndices = map (* wordsize) [1..]
      acc = wordsize * (fromIntegral . length) args
  pushEnvFrame
  modifySI (\si -> si - acc) -- set SI to point after arguments
  mapM_ (\(Atom var, index) -> do
            extendEnv var (StackLocation index)
        ) $ zip args argIndices
  mapM_ (\(Atom var, index) -> do
            extendEnv var (ClosureLocation index)
        ) $ zip freeVars freeVarIndices
  env <- gets env
  emitExpr body
  popEnvFrame
  ret
  resetSI
emitLexpr _ = error "malformed lexpr"

emitExpr :: Expr -> CompC ()
emitExpr (Atom v) = do
  comment $ "variable: " <> v
  getVar v
emitExpr (List [Atom "add1", e]) = do
  comment "add1"
  emitExpr e
  addq (IntOperand (immediateRep (IntExpr 1))) rax
emitExpr (List [Atom "integer->char", e]) = do
  comment "integer->char"
  emitExpr e
  shlq 6 RAX
  orq (IntOperand (tag charRep)) rax
emitExpr (List [Atom "char->integer", e]) = do
  comment "char->integer"
  emitExpr e
  shrq 6 RAX
emitExpr (List [Atom "zero?", e]) = do
  comment "zero?"
  emitExpr e
  cmpq (IntOperand 0) rax
  mkBoolFromFlag
emitExpr (List [Atom "integer?", e]) = do
  comment "integer?"
  emitExpr e
  mkTypePredicate fixnumRep
emitExpr (List [Atom "boolean?", e]) = do
  comment "boolean?"
  emitExpr e
  mkTypePredicate boolRep
emitExpr (List [Atom "=", e1, e2]) = do
  comment "="
  emitExpr e2
  push
  emitExpr e1
  compareWithStack
emitExpr (List [Atom "*", e1, e2]) = do
  comment "*"
  emitExpr e2
  push
  emitExpr e1
  mulWithStack
emitExpr (List [Atom "+", e1, e2]) = do
  comment "+"
  emitExpr e2
  push
  emitExpr e1
  addWithStack
emitExpr (List [Atom "-", e1, e2]) = do
  comment "-"
  emitExpr e2
  push
  emitExpr e1
  subWithStack
emitExpr (List [Atom "let", List bindings, body]) = do
  comment "let"
  mapM_
    (\binding ->
      case binding of
        List [Atom v, e] -> do
          si <- getSI
          emitExpr e
          push
          extendEnv v (StackLocation si)
        _ -> error "bad let syntax")
    bindings
  emitExpr body
emitExpr (List [Atom "letrec", List bindings, body]) = do
  comment "letrec"
  mapM_ (\(i, binding) -> do
      case binding of
        List [Atom v, _] -> do
          si <- getSI
          let index = si - i * wordsize
          -- extendEnv v (StackLocation index)
          extendEnv v (HeapLocation index)
        _ -> error "bad letrec syntax") $ zip [0..] bindings
  mapM_
    (\binding ->
      case binding of
        List [Atom v, e] -> do
          si <- getSI
          heapAlloc e
          -- emitExpr e
          comment $ "setting letrec binding: " <> v
          push
        _ -> error "bad letrec syntax")
    bindings
  emitExpr body
emitExpr (List [Atom "if", cond, conseq, alt]) = do
  comment $ "if " <> display cond <> " " <> display conseq <> " " <> display alt
  l0 <- uniqueLabel' "alt"
  l1 <- uniqueLabel' "conseq"
  emitExpr cond
  cmpq (IntOperand (immediateRep (BoolExpr False))) rax
  je l0
  emitExpr conseq
  jmp l1
  label l0
  emitExpr alt
  label l1

emitExpr (List [Atom "ref", e]) = do
  emitExpr e
  movq (OffsetOperand 0 RAX) rax

emitExpr (List [Atom "cons", e1, e2]) = do
  comment $ "cons " <> display e1 <> " " <> display e2
  movq rsi rax
  orq (i (refTag pairRep)) rax
  addq (i (2 * wordsize)) rsi
  push               -- save ref
  emitExpr e1
  movq rax rbx
  pop                -- restore ref
  movq rbx (0 % RAX) -- save expression 1 result to first slot
  push               -- save ref
  emitExpr e2
  movq rax rbx
  pop
  movq rbx (wordsize % RAX) -- save expression 2 result to second slot

emitExpr (List [Atom "car", e]) = do
  comment "car"
  emitExpr e
  movq (0 % RAX) rax

emitExpr (List [Atom "cdr", e]) = do
  comment "cdr"
  emitExpr e
  movq (wordsize % RAX) rax

emitExpr (List ((Atom "closure") : (Atom lvar) : freeVars)) = do
  comment $ "closure lvar: " <> lvar
  let indices = map (* wordsize) [1..]
      increment = wordsize * (1 + fromIntegral (length freeVars))
  lbl <- lookupLabel lvar
  leaq (PCOffsetOperand lbl) rbx
  movq rbx (0 % RSI)
  mapM_ (\(Atom freeVar, index) -> do
            getVar freeVar
            movq rax (index % RSI))
        $ zip freeVars indices
  movq rsi rax
  orq (IntOperand (refTag closureRep)) rax
  addq (IntOperand increment) rsi

emitExpr (List (f:xs)) = do
  comment "function call"
  env <- gets env
  si <- getSI -- points to one below locals
  -- skip one location for the return point
  decSI
  -- skip another location for the closure pointer
  decSI
  -- push args
  mapM_ (\expr -> do
            emitExpr expr
            push) xs
  callClosure si f

emitExpr e = do
  comment $ "immediateRep: " <> display e
  movq (i (immediateRep e)) rax

mkTypePredicate :: TypeRep -> CompC ()
mkTypePredicate rep = do
  comment "type predicate"
  andq (i (mask rep)) rax
  cmpq (i (tag rep)) rax
  mkBoolFromFlag

immediateRep :: Expr -> Int64
immediateRep (Char c) =
  (toInt64 c `shift` tshift charRep) .|. tag charRep
immediateRep (IntExpr x) =
  (x `shift` tshift fixnumRep) .|. tag fixnumRep
immediateRep (BoolExpr x) =
  let b = if x then 1 else 0
  in (b `shift` tshift boolRep) .|. tag boolRep
immediateRep (List []) = c
  where ConstRep c = emptyListRep
immediateRep e =
  error $ "unhandled expression: " <> show e

toInt64 :: Enum a => a -> Int64
toInt64 = fromIntegral . fromEnum

heapAlloc e = do
  comment $ "heapAlloc " <> display e
  movq rsi rax
  -- orq (i (refTag vectorRep)) rax
  addq (i wordsize) rsi
  push               -- save ref
  emitExpr e
  movq rax rbx
  pop                -- restore ref
  movq rbx (0 % RAX) -- save expression 1 result to first slot

callClosure si f = do
  emitExpr f
  andq (i $ complement $ refTag closureRep) rax -- clear the tag
  movq (0 % RAX) rbx -- rbx contains the pointer to the routine
  movq rdi (si % RSP) -- save rdi
  movq rax rdi
  -- set rsp to two words below return point
  addq (i si) rsp
  -- go to function
  call _rbx
  subq (i si) rsp
  movq (si % RSP) rdi -- restore rdi
