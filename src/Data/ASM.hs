{-# LANGUAGE OverloadedStrings #-}

module Data.ASM where

import qualified Data.Text as T

import Data.Int

data Register = EAX
              | ESP
              | EDI
              | RAX
              | RSP
              | RDI
              | RSI
              | AL
  deriving (Show, Eq)

eax = RegisterOperand EAX
esp = RegisterOperand ESP
edi = RegisterOperand EDI
rax = RegisterOperand RAX
rsp = RegisterOperand RSP
rdi = RegisterOperand RDI
rsi = RegisterOperand RSI
al = RegisterOperand AL

data ASM = MOVQ Operand Operand
         | ADDQ Operand Operand
         | SUBQ Operand Operand
         | CMPQ Operand Operand
         | IMUL Operand Register
         | SALQ Operand Operand
         | ORQ  Operand Operand
         | ANDQ Operand Operand
         | SHRQ  Int64 Register
         | SHLQ  Int64 Register
         | SARQ  Int64 Register
         | SETE Operand
         | LABEL T.Text
         | JMP T.Text
         | JE T.Text
         | RET
  deriving (Show, Eq)

data Operand = RegisterOperand Register
             | IntOperand Int64
             | OffsetOperand Int64 Register
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
formatReg EDI = "%edi"
formatReg RAX = "%rax"
formatReg RSP = "%rsp"
formatReg RDI = "%rdi"
formatReg RSI = "%rsi"
formatReg AL = "%al"

formatASM :: ASM -> T.Text
formatASM (MOVQ src dst) =
  format "movq" src dst
formatASM (ADDQ src dst) =
  format "addq" src dst
formatASM (SUBQ src dst) =
  format "subq" src dst
formatASM (CMPQ src dst) =
  format "cmpq" src dst
formatASM (IMUL op r) =
  format "imul" op (RegisterOperand r)
formatASM (SALQ src dst) =
  format "salq" src dst
formatASM (ORQ  src dst) =
  format "orq" src dst
formatASM (ANDQ  src dst) =
  format "andq" src dst
formatASM (SHRQ x reg) = intRegOp "shrq" x reg
formatASM (SHLQ x reg) = intRegOp "shlq" x reg
formatASM (SARQ  x reg) = intRegOp "sarq" x reg
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
