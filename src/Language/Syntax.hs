{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.Syntax where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Int

type Program = [Expr]

type Ident = T.Text
type Frame = M.Map Ident Expr
type Env = [Frame]
type Error = T.Text
type Eval t = ExceptT Error (State Env) t
type CtxFrame = M.Map Ident Type
type Ctx = [CtxFrame]

data Expr = Atom       Ident
          | Char       Char
          | Str        T.Text
          | IntExpr    Int64
          | DoubleExpr Double
          | BoolExpr   Bool
          | Quote      Expr
          | NativeFunc ([Expr] -> Eval Expr)
          | List       [Expr]

data Type = AtomT
          | BoolT
          | StrT
          | IntT
          | DoubleT
          | ListT
          | NativeT
          | AnyT
          | FuncT [Type]
          deriving (Eq, Show)

true :: Expr
true = BoolExpr True

false :: Expr
false = BoolExpr False

nil :: Expr
nil  = List []

instance Show Expr where
  show (Atom t)       = T.unpack $ "Atom " <> t
  show (Str t)        = T.unpack $ "Str " <> "\"" <> t <> "\""
  show (BoolExpr b)   = "BoolExpr " ++ show b
  show (IntExpr x)    = "IntExpr " ++ show x
  show (DoubleExpr x) = "DoubleExpr " ++ show x
  show (Quote x)      = "Quote " ++ show x
  show (NativeFunc _) = "<native function>"
  show (List xs)      = "List " ++ show xs

display :: Expr -> T.Text
display (Atom t)       = t
display (Str t)        = "\"" <> t <> "\""
display (BoolExpr b)       = T.pack $ show b
display (IntExpr x)    = T.pack $ show x
display (DoubleExpr x) = T.pack $ show x
display (Quote t)      = "'" <> display t
display (NativeFunc _) = "<native function>"
display (List xs)      = "(" <> T.unwords (map display xs) <> ")"

displayT :: Type -> T.Text
displayT = T.pack . show

instance Eq Expr where
  (Atom x)       == (Atom y)       = x == y
  (Str  x)       == (Str  y)       = x == y
  (BoolExpr x)   == (BoolExpr y)   = x == y
  (IntExpr x)    == (IntExpr y)    = x == y
  (DoubleExpr x) == (DoubleExpr y) = x == y
  (Quote x)      == (Quote y)      = x == y
  (List xs)      == (List ys)      = xs == ys
  _              == _              = False
