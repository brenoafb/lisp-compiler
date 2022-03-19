module Main where

import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Language.Parser
import Language.Compiler

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      input <- T.readFile filename
      case parseStr input of
        Left err -> print err
        Right prog ->
          compile prog
