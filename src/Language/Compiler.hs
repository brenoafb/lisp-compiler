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
  -- movl (RegisterOperand RDI) (RegisterOperand RSI)
  emitExpr e
  ret

emitExpr :: Expr -> CompC ()
emitExpr (List [Atom "add1", e]) = do
  emitExpr e
  addl (IntOperand (immediateRep (IntExpr 1))) eax
emitExpr (List [Atom "integer->char", e@(IntExpr _)]) = do
  emitExpr e
  shl 6 EAX
  orl (IntOperand (tag charRep)) eax
emitExpr (List [Atom "char->integer", e@(Char _)]) = do
  emitExpr e
  shr 6 EAX
emitExpr (List [Atom "zero?", e]) = do
  emitExpr e
  cmpl (IntOperand 0) eax
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
  cmpl (IntOperand (immediateRep (BoolExpr False))) eax
  je l0
  emitExpr conseq
  jmp l1
  label l0
  emitExpr alt
  label l1

emitExpr (List [Atom "cons", e1, e2]) = do
  emitExpr e1
  movl eax (0 % RSI)
  emitExpr e2
  movl eax (wordsize % RSI)
  movl rsi eax
  orl (i 1) eax
  addl (i wordsize) rsi

emitExpr (List [Atom "car", e]) = do
  emitExpr e
  movl ((-1) % EAX) eax

emitExpr (List [Atom "cdr", e]) = do
  emitExpr e
  movl (3 % EAX) eax

emitExpr (Atom v) =
  getVar v
emitExpr e = do
  movl (i (immediateRep e)) eax

mkTypePredicate :: TypeRep -> CompC ()
mkTypePredicate rep = do
  andl (i (mask rep)) eax
  cmpl (i (tag rep)) eax
  mkBoolFromFlag

immediateRep :: Expr -> Int32
immediateRep (Char c) =
  (toInt32 c `shift` tshift charRep) .|. (tag charRep)
immediateRep (IntExpr x) =
  (x `shift` tshift fixnumRep) .|. (tag fixnumRep)
immediateRep (BoolExpr x) =
  let b = if x then 1 else 0
  in (b `shift` tshift boolRep) .|. (tag boolRep)
immediateRep (List []) = c
  where ConstRep c = emptyListRep
immediateRep _ = 0xBEEF

toInt32 :: Enum a => a -> Int32
toInt32 = fromIntegral . fromEnum
