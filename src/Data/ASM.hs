{-# LANGUAGE OverloadedStrings #-}

module Data.ASM (ASM(..), Register(..), Operand(..), formatASM) where

import qualified Data.Text as T

import Data.Int

data Register = EAX
              | ESP
              | RAX
              | RSP
              | AL
  deriving (Show, Eq)

data ASM = MOVL Operand Operand
         | ADDL Operand Operand
         | SUBL Operand Operand
         | CMPL Operand Operand
         | IMUL Operand Register
         | SALL Int32 Register
         | ORL  Int32 Register
         | ANDL  Int32 Register
         | SHR  Int32 Register
         | SHL  Int32 Register
         | SAR  Int32 Register
         | SETE Register
         | LABEL T.Text
         | JMP T.Text
         | JE T.Text
         | RET
  deriving (Show, Eq)

data Operand = RegisterOperand Register
             | IntOperand Int32
             | OffsetOperand Int32 Register
             deriving (Eq, Show)

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
formatASM (SALL x reg) = intRegOp "sall" x reg
formatASM (ORL  x reg) = intRegOp "orl" x reg
formatASM (ANDL  x reg) = intRegOp "andl" x reg
formatASM (SHR  x reg) = intRegOp "shr" x reg
formatASM (SHL  x reg) = intRegOp "shl" x reg
formatASM (SAR  x reg) = intRegOp "sar" x reg
formatASM (SETE reg) = "sete" <> " " <> formatReg reg
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
