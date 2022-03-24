{-# LANGUAGE OverloadedStrings #-}

module Data.ASM where

import qualified Data.Text as T

import Data.Int

data Register = EAX
              | ESP
              | EDI
              | RIP
              | RAX
              | RBX
              | RSP
              | RDI
              | RSI
              | AL
  deriving (Show, Eq)

data Operand = RegisterOperand Register
             | StarRegisterOperand Register
             | IntOperand Int64
             | OffsetOperand Int64 Register
             | LabelOperand T.Text
             | PCOffsetOperand T.Text
             deriving (Eq, Show)

eax = RegisterOperand EAX
esp = RegisterOperand ESP
edi = RegisterOperand EDI
rax = RegisterOperand RAX
rbx = RegisterOperand RBX
rsp = RegisterOperand RSP
rdi = RegisterOperand RDI
rsi = RegisterOperand RSI
rip = RegisterOperand RIP
al = RegisterOperand AL

_rbx = StarRegisterOperand RBX

data ASM = MOVQ Operand Operand
         | ADDQ Operand Operand
         | SUBQ Operand Operand
         | CMPQ Operand Operand
         | IMUL Operand Register
         | SALQ Operand Operand
         | ORQ  Operand Operand
         | ANDQ Operand Operand
         | LEAQ Operand Operand
         | SHRQ  Int64 Register
         | SHLQ  Int64 Register
         | SARQ  Int64 Register
         | SETE Operand
         | CALL Operand
         | LABEL T.Text
         | JMP T.Text
         | JE T.Text
         | RET
         | Comment T.Text
  deriving (Show, Eq)

i = IntOperand

l = LabelOperand

i % r = OffsetOperand i r

formatOperand :: Operand -> T.Text
formatOperand (RegisterOperand r) = formatReg r
formatOperand (StarRegisterOperand r) = "*" <> formatReg r
formatOperand (IntOperand x) = "$" <> show' x
formatOperand (OffsetOperand x r) =
  show' x <> "(" <> formatReg r <> ")"
formatOperand (PCOffsetOperand l) =
  l <> "(" <> formatReg RIP <> ")"
formatOperand (LabelOperand l) =
  l

formatReg :: Register -> T.Text
formatReg EAX = "%eax"
formatReg ESP = "%esp"
formatReg EDI = "%edi"
formatReg RAX = "%rax"
formatReg RBX = "%rbx"
formatReg RSP = "%rsp"
formatReg RDI = "%rdi"
formatReg RSI = "%rsi"
formatReg RIP = "%rip"
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
formatASM (LEAQ  src dst) =
  format "leaq" src dst
formatASM (SHRQ x reg) = intRegOp "shrq" x reg
formatASM (SHLQ x reg) = intRegOp "shlq" x reg
formatASM (SARQ  x reg) = intRegOp "sarq" x reg
formatASM (SETE x) = "sete" <> " " <> formatOperand x
formatASM (CALL x) =
  "call " <> formatOperand x
formatASM (JMP l) =
  "jmp " <> l
formatASM (JE l) =
  "je " <> l
formatASM (LABEL l) =
  l <> ":"
formatASM RET =
  "ret"
formatASM (Comment c) = "; " <> c

intRegOp i x reg = i <> " $" <> show' x <> ", " <> formatReg reg

format instr src dst =
  instr <> " " <> formatOperand src <> ", " <> formatOperand dst

show' = T.pack . show
