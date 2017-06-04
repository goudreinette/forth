module Parser where

import           Text.Parsec.Indent
import           Text.Parsec.Prim              (runParserT)
import           Text.ParserCombinators.Parsec hiding (Parser, parse)

type Parser a = IndentParser String () a

data Expr = NamedList Name [Item]
    deriving Show

type Name = String
type Item = String

expr :: Parser Expr
expr = namedList

namedList :: Parser Expr
namedList = do
  b <- withBlock NamedList name item
  spaces
  return b


name :: Parser Name
name = do
   s <- many1 alphaNum
   _ <- char ':'
   spaces
   return s

item :: Parser Item
item = do
   i <- many1 alphaNum
   spaces
   return i


inputText = unlines [
        "listName:",
        "  item1",
        "  item2",
        "  item3"
    ]


parse :: String -> Either ParseError Expr
parse input =
  runIndent $ runParserT expr () "indent" input

parseFile path =
  parse <$> readFile path
