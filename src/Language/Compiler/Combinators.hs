module Language.Compiler.Combinators where

import Control.Monad.State
import Data.ASM
import Data.Int
import Language.Compiler.Constants
import Language.Compiler.Types
import qualified Data.Map as M

emit :: ASM -> CompC ()
emit instr =
  modify (\st -> st { asm = instr : (asm st)})

movq x y = emit $ MOVQ x y
addq x r = emit $ ADDQ x r
subq x r = emit $ SUBQ x r
cmpq x r = emit $ CMPQ x r
label l = emit $ LABEL l
jmp l = emit $ JMP l
je l = emit $ JE l
imul x r = emit $ IMUL x r
salq x r = emit $ SALQ x r
orq x r = emit $ ORQ x r
andq x r = emit $ ANDQ x r
sete r = emit $ SETE r
shrq x r = emit $ SHRQ x r
shlq x r = emit $ SHLQ x r
sarq x r = emit $ SARQ x r
ret = emit RET

push = do
  si <- getSI
  movq rax (OffsetOperand si RSP)
  decSI

pop = do
  incSI
  si <- getSI
  movq (OffsetOperand si RSP) rax

decSI =
  modify (\st -> st { stackIndex = stackIndex st - wordsize })

incSI =
  modify (\st -> st { stackIndex = stackIndex st + wordsize })

getSI = stackIndex <$> get

getVar v = do
  env <- env <$> get
  case M.lookup v env of
    Nothing -> error $ "unbound variable " <> show v
    Just addr -> movq (OffsetOperand addr RSP) rax

mkBoolFromFlag = do
  movq (i 0) rax
  sete al
  salq (i 7) rax
  orq (i 31) rax

compareWithStack = do
  incSI
  si <- stackIndex <$> get
  cmpq (OffsetOperand si RSP) rax
  mkBoolFromFlag

mulWithStack = do
  incSI
  si <- stackIndex <$> get
  imul (si % RSP) RAX
  sarq 2 RAX

addWithStack = do
  incSI
  si <- stackIndex <$> get
  addq (si % RSP) rax

subWithStack = do
  incSI
  si <- stackIndex <$> get
  subq (si % RSP) rax
