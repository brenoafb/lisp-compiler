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
      asmCode = T.unlines $ map (("    " <>) . formatASM) compC
      output = asmHeader <> asmCode
  T.writeFile "output.s" output
  runGCC

runGCC = do
  readProcessWithExitCode "gcc" (words "-g assets/driver.c output.s -o main") ""
  pure ()

compileProgram :: Program -> CompC ()
compileProgram = mapM_ compileExpr

compileExpr e = do
  movq rdi rsi
  emitExpr e
  ret

emitExpr :: Expr -> CompC ()
emitExpr (List [Atom "add1", e]) = do
  emitExpr e
  addq (IntOperand (immediateRep (IntExpr 1))) rax
emitExpr (List [Atom "integer->char", e]) = do
  emitExpr e
  shlq 6 RAX
  orq (IntOperand (tag charRep)) rax
emitExpr (List [Atom "char->integer", e]) = do
  emitExpr e
  shrq 6 RAX
emitExpr (List [Atom "zero?", e]) = do
  emitExpr e
  cmpq (IntOperand 0) rax
  mkBoolFromFlag
emitExpr (List [Atom "integer?", e]) = do
  emitExpr e
  mkTypePredicate fixnumRep
emitExpr (List [Atom "boolean?", e]) = do
  emitExpr e
  mkTypePredicate boolRep
emitExpr (List [Atom "=", e1, e2]) = do
  emitExpr e2
  push
  emitExpr e1
  compareWithStack
emitExpr (List [Atom "*", e1, e2]) = do
  emitExpr e2
  push
  emitExpr e1
  mulWithStack
emitExpr (List [Atom "+", e1, e2]) = do
  emitExpr e2
  push
  emitExpr e1
  addWithStack
emitExpr (List [Atom "-", e1, e2]) = do
  emitExpr e2
  push
  emitExpr e1
  subWithStack
emitExpr (List [Atom "let", List bindings, body]) = do
  mapM_
    (\binding ->
      case binding of
        List [Atom v, e] -> do
          si <- getSI
          emitExpr e
          push
          extendEnv v si
        _ -> error "bad let syntax")
    bindings
  emitExpr body
emitExpr (List [Atom "if", cond, conseq, alt]) = do
  l0 <- uniqueLabel
  l1 <- uniqueLabel
  emitExpr cond
  cmpq (IntOperand (immediateRep (BoolExpr False))) rax
  je l0
  emitExpr conseq
  jmp l1
  label l0
  emitExpr alt
  label l1

emitExpr (List [Atom "cons", e1, e2]) = do
  emitExpr e1
  movq rax (0 % RSI)
  emitExpr e2
  movq rax (wordsize % RSI)
  movq rsi rax
  orq (i 1) rax
  addq (i wordsize) rsi

emitExpr (List [Atom "car", e]) = do
  emitExpr e
  movq ((-1) % RAX) rax

emitExpr (List [Atom "cdr", e]) = do
  emitExpr e
  movq (7 % RAX) rax

emitExpr (Atom v) =
  getVar v
emitExpr e = do
  movq (i (immediateRep e)) rax

mkTypePredicate :: TypeRep -> CompC ()
mkTypePredicate rep = do
  andq (i (mask rep)) rax
  cmpq (i (tag rep)) rax
  mkBoolFromFlag

immediateRep :: Expr -> Int64
immediateRep (Char c) =
  (toInt64 c `shift` tshift charRep) .|. (tag charRep)
immediateRep (IntExpr x) =
  (x `shift` tshift fixnumRep) .|. (tag fixnumRep)
immediateRep (BoolExpr x) =
  let b = if x then 1 else 0
  in (b `shift` tshift boolRep) .|. (tag boolRep)
immediateRep (List []) = c
  where ConstRep c = emptyListRep
immediateRep e =
  error $ "unhandled expression: " <> show e

toInt64 :: Enum a => a -> Int64
toInt64 = fromIntegral . fromEnum
