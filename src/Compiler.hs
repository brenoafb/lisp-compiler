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
andl x r = emit $ ANDL x r
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
  orl (tag charRep) EAX
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
compileExpr (List [Atom "integer?", e]) = do
  compileExpr e
  mkTypePredicate fixnumRep
compileExpr (List [Atom "boolean?", e]) = do
  compileExpr e
  mkTypePredicate boolRep
compileExpr e = do
  movl (immediateRep e) EAX

mkTypePredicate :: TypeRep -> CompC ()
mkTypePredicate rep = do
  andl (mask rep) EAX
  cmpl (tag rep) EAX
  movl 0 EAX
  sete AL
  sall 7 EAX
  orl 31 EAX

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

data TypeRep =
  TaggedRep { mask   :: Int32
            , tag    :: Int32
            , tshift :: Int
            }
  | ConstRep Int32
  deriving (Eq, Show)


fixnumRep =
  TaggedRep { mask  = 3
          , tag   = 0
          , tshift = 2
          }

charRep =
  TaggedRep { mask  = 0xFF
          , tag   = 0x0E
          , tshift = 8
          }

boolRep =
  TaggedRep { mask  = 0x3F
          , tag   = 0x1F
          , tshift = 7
          }

emptyListRep = ConstRep 0x2F
