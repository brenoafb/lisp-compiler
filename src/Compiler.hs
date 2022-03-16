{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler (compile) where

import Control.Monad.State
import Control.Monad.Identity

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (intercalate)
import Data.Bits
import Data.Int

import System.Process
import System.Exit

import Syntax
import ASM

type CompC = StateT [ASM] Identity

runCompC c = reverse $ runIdentity (execStateT c [])

header =
  T.unlines [ "    .text"
            , "    .p2align 4,,15"
            , ".global scheme_entry"
            , "    .type lisp_entry, @function"
            , "scheme_entry:"
            ]

compile prog = do
  let compC = runCompC $ compileProgram prog
      asm = T.unlines $ map (("    " <>) . formatASM) compC
      output = header <> asm
  T.writeFile "output.s" output
  runGCC

runGCC = do
  readProcessWithExitCode "gcc" (words "assets/driver.c output.s -o main") ""
  pure ()

emit :: ASM -> CompC ()
emit instr = modify (instr :)

movl x r = emit $ MOVL x r
addl x r = emit $ ADDL x r
cmpl x r = emit $ CMPL x r
sall x r = emit $ SALL x r
orl x r = emit $ ORL x r
sete r = emit $ SETE r
shr x r = emit $ SHR x r
shl x r = emit $ SHL x r
ret = emit RET

compileProgram :: Program -> CompC ()
compileProgram = mapM_ compileExpr'

compileExpr' e = do
  compileExpr e
  ret

compileExpr :: Expr -> CompC ()
compileExpr (List [Atom "add1", e]) = do
  compileExpr e
  addl (immediateRep (IntExpr 1)) EAX
compileExpr (List [Atom "integer->char", e@(IntExpr _)]) = do
  compileExpr e
  shl 6 EAX
  orl char_tag EAX
compileExpr (List [Atom "char->integer", e@(Char _)]) = do
  compileExpr e
  shr 6 EAX
compileExpr (List [Atom "zero?", e]) = do
  compileExpr e
  cmpl 0 EAX
  movl 0 EAX
  sete AL
  sall 7 EAX
  orl 31 EAX
compileExpr e = do
  movl (immediateRep e) EAX

immediateRep :: Expr -> Int32
immediateRep (Char c) =
  (toInt32 c `shift` char_shift) .|. char_tag
immediateRep (IntExpr x) =
  (x `shift` fixnum_shift) .|. fixnum_tag
immediateRep (BoolExpr x) =
  let b = if x then 1 else 0
  in (b `shift` bool_shift) .|. bool_tag
immediateRep (List []) = empty_list
immediateRep _ = 0xBEEF


toInt32 = fromIntegral . fromEnum

fixnum_mask  = 3
fixnum_tag   = 0
fixnum_shift = 2
char_mask    = 0xFF
char_tag     = 0x0E
char_shift   = 8
bool_mask    = 0x3F
bool_tag     = 0x1F
bool_shift   = 7
empty_list   = 0x2F
