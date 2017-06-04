module Parser where

import           Text.Parsec.Indent
import           Text.Parsec.Prim              (runParserT)
import           Text.ParserCombinators.Parsec hiding (Parser, parse)
import           Types

type Parser a = IndentParser String () a


expr :: Parser Object
expr = namedList <|> identifier


namedList :: Parser Object
namedList = do
  b <- withBlock NamedList name identifier
  spaces
  return b

name :: Parser Name
name = do
   s <- many1 alphaNum
   _ <- char ':'
   spaces
   return s

identifier :: Parser Object
identifier = do
   i <- many1 alphaNum
   spaces
   return $ Identifier i



parse :: String -> Either ParseError Object
parse input =
  runIndent $ runParserT expr () "indent" input

parseFile path =
  parse <$> readFile path
