{-# LANGUAGE OverloadedStrings #-}

module ASM where

import qualified Data.Text as T

import Data.Bits
import Data.Int

data Register = EAX
              | AL
  deriving (Show, Eq)

data ASM = MOVL Int32 Register
         | ADDL Int32 Register
         | CMPL Int32 Register
         | SALL Int32 Register
         | ORL  Int32 Register
         | SHR  Int32 Register
         | SHL  Int32 Register
         | SETE Register
         | RET
  deriving (Show, Eq)

formatReg :: Register -> T.Text
formatReg EAX = "%eax"
formatReg AL = "%al"

formatASM :: ASM -> T.Text
formatASM (MOVL x reg) = intRegOp "movl" x reg
formatASM (ADDL x reg) = intRegOp "addl" x reg
formatASM (CMPL x reg) = intRegOp "cmpl" x reg
formatASM (SALL x reg) = intRegOp "sall" x reg
formatASM (ORL  x reg) = intRegOp "orl" x reg
formatASM (SHR  x reg) = intRegOp "shr" x reg
formatASM (SHL  x reg) = intRegOp "shl" x reg
formatASM (SETE reg) = "sete" <> " " <> formatReg reg
formatASM RET =
  "ret"

intRegOp i x reg = i <> " $" <> show' x <> ", " <> formatReg reg

show' = T.pack . show
