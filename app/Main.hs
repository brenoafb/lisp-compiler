module Main where

import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Pretty.Simple (pPrint)

import Language.Parser
import Language.Compiler
import Language.Compiler.Desugar
import Language.Syntax

main :: IO ()
main = do
  getArgs >>= handler

handler [filename] = do
  input <- T.readFile filename
  case parseStr input of
    Left err -> print err
    Right prog -> do
      let desugared = desugarExpr prog
      pPrint desugared
      T.putStrLn $ display desugared
      compile desugared

handler _ =
  putStrLn "usage: stack run -- [filename]"
