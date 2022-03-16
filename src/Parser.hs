{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Syntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Text as T

languageDef =
  emptyDef { Token.commentLine     = ";"
           , Token.identStart      = alphaNum <|> oneOf ":!#$%&*+./<=>?@\\^|-~_"
           , Token.identLetter     = alphaNum <|> oneOf ":!#$%&*+./<=>?@\\^|-~_"
           , Token.reservedNames   = ["true", "false"]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
parens = Token.parens lexer
int = fromIntegral <$> Token.natural lexer
double = Token.float lexer
whiteSpace = Token.whiteSpace lexer
comma = Token.comma lexer
stringLiteral = Token.stringLiteral lexer
reservedOp = Token.reservedOp lexer
reserved = Token.reserved lexer

parseStr :: T.Text -> Either ParseError Program
parseStr str = parse program "" (T.unpack str)

program :: Parser Program
program = whiteSpace >> many expr

expr :: Parser Expr
expr = try doubleExpr
  <|> try intExpr
  <|> boolExpr
  <|> quote
  <|> atom
  <|> stringExpr
  <|> list

boolExpr :: Parser Expr
boolExpr =
  BoolExpr <$> ((reserved "true" >> pure True) <|> (reserved "false" >> pure False))

doubleExpr :: Parser Expr
doubleExpr = DoubleExpr <$> double

intExpr :: Parser Expr
intExpr = IntExpr <$> int

atom :: Parser Expr
atom = Atom . T.pack <$> identifier

quote :: Parser Expr
quote = do
  _ <- char '\''
  Quote <$> expr

list :: Parser Expr
list = List <$> parens exprs
  where exprs = many expr

stringExpr :: Parser Expr
stringExpr = Str . T.pack <$> stringLiteral
