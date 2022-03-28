{-# LANGUAGE OverloadedStrings #-}
module Language.Compiler.Desugar where

import Data.Generics
import Data.List
import Data.Maybe (fromJust)
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T

import Language.Syntax

type SEnv = M.HashMap Expr Ident

desugarExpr :: Expr -> Expr
desugarExpr = normalizeExpr . normalizeLambdas

normalizeLambdas :: Expr -> Expr
normalizeLambdas = everywhere (mkT annotateLambdaFreeVariables)

normalizeExpr :: Expr -> Expr
normalizeExpr e =
  List [Atom "labels", List codeLabels, body]
    where lambdas = findLambdas e
          labels = map (\n -> T.pack $ "f" <> show n) [0..]
          labelPairs = zip lambdas labels
          codeLabels = map (\(lambda, label) -> List [Atom label, lambdaToCode lambda]) labelPairs
          senv = M.fromList labelPairs
          body = everywhere (mkT (lambdaToClosure senv)) e

lambdaToCode :: Expr -> Expr
lambdaToCode (List [Atom "lambda", args, freeVars, body]) =
  List [Atom "code", args, freeVars, body]
lambdaToCode e = e

annotateLambdaFreeVariables :: Expr -> Expr
annotateLambdaFreeVariables (List [Atom "lambda", List args, body]) =
  let freeVars = map Atom $ findFreeVariables args' body
      args' = map (\(Atom x) -> x) args -- TODO make this not partial
   in List [Atom "lambda", List args, List freeVars, body]
annotateLambdaFreeVariables e = e

findFreeVariables :: [Ident] -> Expr -> [Ident]
findFreeVariables args e = atoms \\ (args ++ primitives)
  where atoms = findAtoms e
        primitives = -- TODO find a better solution for this
          ["cons", "ref", "*", "+", "-", "zero?", "if"]

findAtoms :: Expr -> [Ident]
findAtoms = nub . everything (<>) ([] `mkQ` atom)
  where atom (Atom e) = [e]
        atom _ = []

findLambdas :: Expr -> [Expr]
findLambdas = everything (<>) ([] `mkQ` lambda)
  where lambda e
          | isLambda e = [e]
          | otherwise = []

lambdaToClosure :: SEnv -> Expr -> Expr
lambdaToClosure senv l@(List [Atom "lambda", List args, List freeVars, body]) =
  List $ [Atom "closure", Atom f] ++ freeVars
    where f = fromJust $ M.lookup l senv -- TODO make not partial
lambdaToClosure _ e = e

isLambda (List (Atom "lambda":_)) = True
isLambda _ = False
