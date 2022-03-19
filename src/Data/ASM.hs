{-# LANGUAGE OverloadedStrings #-}

module Data.ASM where

import qualified Data.Text as T

import Data.Int

data Register = EAX
              | ESP
              | RAX
              | RSP
              | RDI
              | RSI
              | AL
  deriving (Show, Eq)

eax = RegisterOperand EAX
esp = RegisterOperand ESP
rax = RegisterOperand RAX
rsp = RegisterOperand RSP
rdi = RegisterOperand RDI
rsi = RegisterOperand RSI
al = RegisterOperand AL

data ASM = MOVL Operand Operand
         | ADDL Operand Operand
         | SUBL Operand Operand
         | CMPL Operand Operand
         | IMUL Operand Register
         | SALL Operand Operand
         | ORL  Operand Operand
         | ANDL Operand Operand
         | SHR  Int32 Register
         | SHL  Int32 Register
         | SAR  Int32 Register
         | SETE Operand
         | LABEL T.Text
         | JMP T.Text
         | JE T.Text
         | RET
  deriving (Show, Eq)

data Operand = RegisterOperand Register
             | IntOperand Int32
             | OffsetOperand Int32 Register
             deriving (Eq, Show)

i = IntOperand

i % r = OffsetOperand i r

formatOperand :: Operand -> T.Text
formatOperand (RegisterOperand r) = formatReg r
formatOperand (IntOperand x) = "$" <> show' x
formatOperand (OffsetOperand x r) =
  show' x <> "(" <> formatReg r <> ")"

formatReg :: Register -> T.Text
formatReg EAX = "%eax"
formatReg ESP = "%esp"
formatReg RAX = "%rax"
formatReg RSP = "%rsp"
formatReg RDI = "%rdi"
formatReg RSI = "%rsi"
formatReg AL = "%al"

formatASM :: ASM -> T.Text
formatASM (MOVL src dst) =
  format "movl" src dst
formatASM (ADDL src dst) =
  format "addl" src dst
formatASM (SUBL src dst) =
  format "subl" src dst
formatASM (CMPL src dst) =
  format "cmpl" src dst
formatASM (IMUL op r) =
  format "imul" op (RegisterOperand r)
formatASM (SALL src dst) =
  format "sall" src dst
formatASM (ORL  src dst) =
  format "orl" src dst
formatASM (ANDL  src dst) =
  format "andl" src dst
formatASM (SHR  x reg) = intRegOp "shr" x reg
formatASM (SHL  x reg) = intRegOp "shl" x reg
formatASM (SAR  x reg) = intRegOp "sar" x reg
formatASM (SETE x) = "sete" <> " " <> formatOperand x
formatASM (JMP l) =
  "jmp " <> l
formatASM (JE l) =
  "je " <> l
formatASM (LABEL l) =
  l <> ":"
formatASM RET =
  "ret"

intRegOp i x reg = i <> " $" <> show' x <> ", " <> formatReg reg

format instr src dst =
  instr <> " " <> formatOperand src <> ", " <> formatOperand dst

show' = T.pack . show
