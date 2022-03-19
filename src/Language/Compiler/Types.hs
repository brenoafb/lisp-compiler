module Language.Compiler.Types where

import Data.ASM
import Data.Int

import Control.Monad.State
import Control.Monad.Identity

import qualified Data.Map as M
import qualified Data.Text as T

data CompState =
  CompState { asm :: [ASM]
            , stackIndex :: Int32
            , env :: Env
            , labelCounter :: Int
            }
  deriving (Eq, Show)

type CompC = StateT CompState Identity

type Ident = T.Text

type Env = M.Map Ident Int32
emptyEnv = M.empty

extendEnv :: Ident -> Int32 -> CompC ()
extendEnv v i =
  modify (\st -> st { env = M.insert v i (env st) })

incrementLabelCounter =
  modify (\st -> st { labelCounter = labelCounter st + 1 })

uniqueLabel :: CompC T.Text
uniqueLabel = do
  counter <- labelCounter <$> get
  incrementLabelCounter
  pure . T.pack $ "L" <> show counter

runCompC c =
  reverse . asm . runIdentity $ execStateT c initialState
  where initialState =
          CompState { asm = []
                    , stackIndex = (-8)
                    , env = emptyEnv
                    , labelCounter = 0
                    }

data TypeRep =
  TaggedRep { mask   :: Int32
            , tag    :: Int32
            , tshift :: Int
            }
  | ConstRep Int32
  deriving (Eq, Show)
