module Language.Compiler.Types where

import Data.ASM
import Data.Int

import Control.Monad.State
import Control.Monad.Identity

data CompState =
  CompState { asm :: [ASM]
            , stackIndex :: Int32
            }
  deriving (Eq, Show)

type CompC = StateT CompState Identity

runCompC c =
  reverse . asm . runIdentity $ execStateT c initialState
  where initialState = CompState [] (-8)

data TypeRep =
  TaggedRep { mask   :: Int32
            , tag    :: Int32
            , tshift :: Int
            }
  | ConstRep Int32
  deriving (Eq, Show)
