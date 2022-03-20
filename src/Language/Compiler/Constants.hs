{-# LANGUAGE OverloadedStrings #-}
module Language.Compiler.Constants where

import Data.Int
import Language.Compiler.Types

import qualified Data.Text as T

fixnumRep =
  ImmRep { mask  = 3
         , tag   = 0
         , tshift = 2
         }

charRep =
  ImmRep { mask  = 0xFF
         , tag   = 0x0E
         , tshift = 8
         }

boolRep =
  ImmRep { mask  = 0x3F
         , tag   = 0x1F
         , tshift = 7
         }

pairRep =
  RefRep 0x1

vectorRep =
  RefRep 0x2

stringRep =
  RefRep 0x3

symbolRep =
  RefRep 0x5

closureRep =
  RefRep 0x6

emptyListRep = ConstRep 0x2F

wordsize :: Int64
wordsize = 8

asmHeader =
  T.unlines [ "    .text"
            , "    .p2align 4,,15"
            , ".global scheme_entry"
            , "    .type lisp_entry, @function"
            , "scheme_entry:"
            ]
