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
movli x r = movl (IntOperand x) (RegisterOperand r)
movlo  x r1 r2 = movl (OffsetOperand x r1) (RegisterOperand r2)
movlo' r1 r2 x = movl (RegisterOperand r1) (OffsetOperand x r2)

addl x r = emit $ ADDL x r
addli x r = addl (IntOperand x) (RegisterOperand r)
addlo x r1 r2 = addl (OffsetOperand x r1) (RegisterOperand r2)

subl x r = emit $ SUBL x r
subli x r = subl (IntOperand x) (RegisterOperand r)
sublo x r1 r2 = subl (OffsetOperand x r1) (RegisterOperand r2)

cmpl x r = emit $ CMPL x r
cmpli x r = cmpl (IntOperand x) (RegisterOperand r)
cmplo x r1 r2 = cmpl (OffsetOperand x r1) (RegisterOperand r2)

sall x r = emit $ SALL x r
orl x r = emit $ ORL x r
andl x r = emit $ ANDL x r
sete r = emit $ SETE r
shr x r = emit $ SHR x r
shl x r = emit $ SHL x r
ret = emit RET
push = do
  si <- getSI
  movlo' EAX RSP si
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
    Just addr -> movlo addr RSP EAX

mkBoolFromFlag = do
  movli 0 EAX
  sete AL
  sall 7 EAX
  orl 31 EAX

compareWithStack = do
  incSI
  si <- stackIndex <$> get
  cmplo si RSP EAX
  mkBoolFromFlag

addWithStack = do
  incSI
  si <- stackIndex <$> get
  addlo si RSP EAX

subWithStack = do
  incSI
  si <- stackIndex <$> get
  sublo si RSP EAX
