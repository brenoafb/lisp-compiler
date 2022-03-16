{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler where

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

data Register = EAX
  deriving (Show, Eq)

data ASM = MOVL Int32 Register
         | RET
  deriving (Show, Eq)

formatReg :: Register -> T.Text
formatReg EAX = "%eax"

formatASM :: ASM -> T.Text
formatASM (MOVL x reg) =
  "movl    $" <> show' x <> ", " <> formatReg reg
formatASM RET =
  "ret"

show' = T.pack . show

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
      asm = T.unlines $ map formatASM compC
      output = header <> asm
  T.writeFile "output.s" output
  runGCC

runGCC = do
  readProcessWithExitCode "gcc" (words "assets/driver.c output.s -o main") ""
  pure ()

emit :: ASM -> CompC ()
emit instr = modify (instr :)

compileProgram :: Program -> CompC ()
compileProgram = mapM_ compileExpr

compileExpr :: Expr -> CompC ()
compileExpr e = do
  emit $ MOVL (immediateRep e) EAX
  emit $ RET


immediateRep :: Expr -> Int32
immediateRep (IntExpr x) = x `shift` 2
immediateRep (List []) = 0x2F
