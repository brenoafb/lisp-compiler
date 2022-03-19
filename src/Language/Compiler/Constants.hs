{-# LANGUAGE OverloadedStrings #-}
module Language.Compiler.Constants where

import Data.Int
import Language.Compiler.Types

import qualified Data.Text as T

fixnumRep =
  TaggedRep { mask  = 3
          , tag   = 0
          , tshift = 2
          }

charRep =
  TaggedRep { mask  = 0xFF
          , tag   = 0x0E
          , tshift = 8
          }

boolRep =
  TaggedRep { mask  = 0x3F
          , tag   = 0x1F
          , tshift = 7
          }

emptyListRep = ConstRep 0x2F

wordsize :: Int32
wordsize = 8

header =
  T.unlines [ "    .text"
            , "    .p2align 4,,15"
            , ".global scheme_entry"
            , "    .type lisp_entry, @function"
            , "scheme_entry:"
            ]
