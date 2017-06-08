module Parser where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Number (int)
import           Types

-- Parsers
exprs :: Parser [Val]
exprs = many expr

expr :: Parser Val
expr = do
  spaces
  e <- word <|> number
  spaces
  return e

number :: Parser Val
number = Number <$> int

word :: Parser Val
word = do
  w <- many1 (oneOf ".+-/*:;!@#$%^&*<>" <|> letter)
  return $ Symbol w

lambda :: Parser Val
lambda = do
  char '['
  es <- exprs
  char ']'
  return $ makeWord es



-- API
parseLine =
  parse exprs "forth"

parseFile path =
  parseLine <$> readFile path
