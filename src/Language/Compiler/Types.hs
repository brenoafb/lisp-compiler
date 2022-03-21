module Language.Compiler.Types where

import Data.ASM
import Data.Int

import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe (isJust, fromMaybe)

import qualified Data.Map as M
import qualified Data.Text as T

data CompState =
  CompState { asm :: [ASM]
            , stackIndex :: Int64
            , env :: Env
            , labels :: Labels
            , labelCounter :: Int
            }
  deriving (Eq, Show)

type CompC = StateT CompState Identity

type Ident = T.Text
type Label = T.Text

type Env = [M.Map Ident Int64]

emptyEnv = [M.empty]

extendEnv :: Ident -> Int64 -> CompC ()
extendEnv v i =
  let go env' =
        case env' of
          [] -> error "empty env"
          e:es -> M.insert v i e : es
  in modify (\st -> st { env = go (env st) })

pushEnvFrame :: CompC ()
pushEnvFrame =
  modify (\st -> st { env = M.empty : env st })

popEnvFrame :: CompC ()
popEnvFrame =
  let go env' =
        case env' of
          [] -> error "empty env"
          _:es -> es
  in modify (\st -> st { env = go $ env st })

frameLookup :: Ident -> Env -> [Int64]
frameLookup v e =
  concatMap (maybe [] (: []) . M.lookup v) e

type Labels = M.Map Ident Label

emptyLabels = M.empty

addLabel :: Ident -> Label -> CompC ()
addLabel v l =
  modify (\st -> st { labels = M.insert v l (labels st) })

lookupLabel :: Ident -> CompC Label
lookupLabel x = do
  labels <- labels <$> get
  case M.lookup x labels of
    Nothing -> error $ "undefined label variable " <> (show x)
    Just l -> pure l

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
                    , labels = emptyLabels
                    , labelCounter = 0
                    }

data TypeRep =
    ImmRep { mask   :: Int64
           , tag    :: Int64
           , tshift :: Int
           }
  | RefRep { refTag :: Int64 }
  | ConstRep Int64
  deriving (Eq, Show)
