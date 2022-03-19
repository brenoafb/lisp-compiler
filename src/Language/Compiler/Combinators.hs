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

movl x y = emit $ MOVL x y
addl x r = emit $ ADDL x r
subl x r = emit $ SUBL x r
cmpl x r = emit $ CMPL x r
label l = emit $ LABEL l
jmp l = emit $ JMP l
je l = emit $ JE l
imul x r = emit $ IMUL x r
sall x r = emit $ SALL x r
orl x r = emit $ ORL x r
andl x r = emit $ ANDL x r
sete r = emit $ SETE r
shr x r = emit $ SHR x r
shl x r = emit $ SHL x r
sar x r = emit $ SAR x r
ret = emit RET
push = do
  si <- getSI
  movl eax (OffsetOperand si RSP)
  decSI

decSI =
  modify (\st -> st { stackIndex = stackIndex st - wordsize })

incSI =
  modify (\st -> st { stackIndex = stackIndex st + wordsize })

getSI = stackIndex <$> get

getVar v = do
  env <- env <$> get
  case M.lookup v env of
    Nothing -> error "unbound variable"
    Just addr -> movl (OffsetOperand addr RSP) eax

mkBoolFromFlag = do
  movl (i 0) eax
  sete al
  sall (i 7) eax
  orl (i 31) eax

compareWithStack = do
  incSI
  si <- stackIndex <$> get
  cmpl (OffsetOperand si RSP) eax
  mkBoolFromFlag

mulWithStack = do
  incSI
  si <- stackIndex <$> get
  imul (si % RSP) EAX
  sar 2 EAX

addWithStack = do
  incSI
  si <- stackIndex <$> get
  addl (si % RSP) eax

subWithStack = do
  incSI
  si <- stackIndex <$> get
  subl (si % RSP) eax
