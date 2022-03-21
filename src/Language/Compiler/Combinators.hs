{-# LANGUAGE FlexibleContexts #-}
module Language.Compiler.Combinators where

import Control.Monad.State
import Control.Applicative
import Data.ASM
import Data.Int
import Language.Compiler.Constants
import Language.Compiler.Types
import qualified Data.Map as M

emit :: ASM -> CompC ()
emit instr =
  modify (\st -> st { asm = instr : asm st})

movq x y = emit $ MOVQ x y
addq x r = emit $ ADDQ x r
subq x r = emit $ SUBQ x r
cmpq x r = emit $ CMPQ x r
call l = emit $ CALL l
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

resetSI :: CompC ()
resetSI = setSI (-wordsize)

decSI :: CompC ()
decSI = modifySI (\si -> si - wordsize)

incSI :: CompC ()
incSI = modifySI (+ wordsize)

getSI :: CompC Int64
getSI = gets stackIndex

setSI :: Int64 -> CompC ()
setSI si = modifySI (const si)

modifySI :: (Int64 -> Int64) -> CompC ()
modifySI f =
  modify (\st -> st { stackIndex = f $ stackIndex st })

getVar v = do
  env <- gets env
  case frameLookup v env of
    [] -> error $ "unbound variable " <> show v
    addr:_ -> movq (OffsetOperand addr RSP) rax

mkBoolFromFlag = do
  movq (i 0) rax
  sete al
  salq (i 7) rax
  orq (i 31) rax

compareWithStack = do
  incSI
  si <- gets stackIndex
  cmpq (OffsetOperand si RSP) rax
  mkBoolFromFlag

mulWithStack = do
  incSI
  si <- gets stackIndex
  imul (si % RSP) RAX
  sarq 2 RAX

addWithStack = do
  incSI
  si <- gets stackIndex
  addq (si % RSP) rax

subWithStack = do
  incSI
  si <- gets stackIndex
  subq (si % RSP) rax
