{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Compiler (compile) where

import Language.Compiler.Combinators
import Language.Compiler.Constants
import Language.Compiler.Types

import Control.Monad.State
import Control.Monad.Identity

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (intercalate)
import Data.Bits
import Data.Int

import System.Process
import System.Exit

import Language.Syntax
import Data.ASM

compile prog = do
  let compC = runCompC $ compileProgram prog
      asm = T.unlines $ map (("    " <>) . formatASM) compC
      output = header <> asm
  T.writeFile "output.s" output
  runGCC

runGCC = do
  readProcessWithExitCode "gcc" (words "-g assets/driver.c output.s -o main") ""
  pure ()

compileProgram :: Program -> CompC ()
compileProgram = mapM_ compileExpr'

compileExpr' e = do
  compileExpr e
  ret

compileExpr :: Expr -> CompC ()
compileExpr (List [Atom "add1", e]) = do
  compileExpr e
  addli (immediateRep (IntExpr 1)) EAX
compileExpr (List [Atom "integer->char", e@(IntExpr _)]) = do
  compileExpr e
  shl 6 EAX
  orl (tag charRep) EAX
compileExpr (List [Atom "char->integer", e@(Char _)]) = do
  compileExpr e
  shr 6 EAX
compileExpr (List [Atom "zero?", e]) = do
  compileExpr e
  cmpli 0 EAX
  mkBoolFromFlag
compileExpr (List [Atom "integer?", e]) = do
  compileExpr e
  mkTypePredicate fixnumRep
compileExpr (List [Atom "boolean?", e]) = do
  compileExpr e
  mkTypePredicate boolRep
compileExpr (List [Atom "=", e1, e2]) = do
  compileExpr e2
  push
  compileExpr e1
  compareWithStack
compileExpr (List [Atom "+", e1, e2]) = do
  compileExpr e2
  push
  compileExpr e1
  addWithStack
compileExpr (List [Atom "-", e1, e2]) = do
  compileExpr e2
  push
  compileExpr e1
  subWithStack
compileExpr e = do
  movli (immediateRep e) EAX

mkTypePredicate :: TypeRep -> CompC ()
mkTypePredicate rep = do
  andl (mask rep) EAX
  cmpli (tag rep) EAX
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
