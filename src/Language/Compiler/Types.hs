module Language.Compiler.Types where

import Data.ASM
import Data.Int

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Data.Maybe (isJust, fromMaybe)

import qualified Data.Map as M
import qualified Data.Text as T

data CompState =
  CompState { asm :: [ASM]
            , stackIndex :: Int64
            , env :: Env
            , labelCounter :: Int
            }
  deriving (Eq, Show)

data CompError = CompilerError
  deriving (Eq, Show)

type CompC = StateT CompState (ExceptT CompError Identity)

type Ident = T.Text
type Label = T.Text

data Location = StackLocation   Int64
              | ClosureLocation Int64
              | LabelLocation   Label
              deriving (Eq, Show)

type Env = [M.Map Ident Location]

emptyEnv = [M.empty]

extendEnv :: Ident -> Location -> CompC ()
extendEnv v loc =
  let go env' =
        case env' of
          [] -> error "empty env"
          e:es -> M.insert v loc e : es
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

frameLookup :: Ident -> Env -> [Location]
frameLookup v e =
  concatMap (maybe [] (: []) . M.lookup v) e

envLookup :: Ident -> CompC Location
envLookup ident = do
  env <- gets env
  case frameLookup ident env of
    [] -> error $ "Unbound variable: " <> show ident
    (x:xs) -> pure x

lookupStack :: Ident -> CompC Int64
lookupStack x = do
  lookup <- envLookup x
  case lookup of
    StackLocation l -> pure l
    _ -> error $ "undefined label variable " <> (show x)

lookupLabel :: Ident -> CompC Label
lookupLabel x = do
  lookup <- envLookup x
  case lookup of
    LabelLocation l -> pure l
    _ -> error $ "undefined label variable " <> (show x)

incrementLabelCounter =
  modify (\st -> st { labelCounter = labelCounter st + 1 })

uniqueLabel :: CompC T.Text
uniqueLabel = do
  counter <- labelCounter <$> get
  incrementLabelCounter
  pure . T.pack $ "L" <> show counter

runCompC :: CompC a -> [ASM]
runCompC c =
  case runIdentity $ runExceptT $ execStateT c initialState of
    Left err -> undefined
    Right compState -> reverse $ asm compState
  where initialState =
          CompState { asm = []
                    , stackIndex = (-8)
                    , env = emptyEnv
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
