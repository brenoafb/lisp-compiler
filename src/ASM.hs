{-# LANGUAGE OverloadedStrings #-}

module ASM where

import qualified Data.Text as T

import Data.Bits
import Data.Int

data Register = EAX
              | ESP
              | RAX
              | RSP
              | AL
  deriving (Show, Eq)

data ASM = MOVL Operand Operand
         | ADDL Operand Operand
         | CMPL Int32 Register
         | SALL Int32 Register
         | ORL  Int32 Register
         | ANDL  Int32 Register
         | SHR  Int32 Register
         | SHL  Int32 Register
         | SETE Register
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
formatASM (MOVL opSrc opDst) =
  "movl" <> " " <> formatOperand opSrc <> ", " <> formatOperand opDst
formatASM (ADDL opSrc opDst) =
  "addl" <> " " <> formatOperand opSrc <> ", " <> formatOperand opDst
formatASM (CMPL x reg) = intRegOp "cmpl" x reg
formatASM (SALL x reg) = intRegOp "sall" x reg
formatASM (ORL  x reg) = intRegOp "orl" x reg
formatASM (ANDL  x reg) = intRegOp "andl" x reg
formatASM (SHR  x reg) = intRegOp "shr" x reg
formatASM (SHL  x reg) = intRegOp "shl" x reg
formatASM (SETE reg) = "sete" <> " " <> formatReg reg
formatASM RET =
  "ret"

intRegOp i x reg = i <> " $" <> show' x <> ", " <> formatReg reg

show' = T.pack . show
