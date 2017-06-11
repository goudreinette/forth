module Parser where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Number (int)
import           Types

wordChars = ".+-/*:;!@#$%^&*<>="

-- Parsers
exprs :: Parser [Val]
exprs = many expr

expr :: Parser Val
expr = between spaces spaces $
  bool <|> word <|> number <|> quotation


number :: Parser Val
number = Number <$> int

word :: Parser Val
word =
  Symbol <$> many1 (oneOf wordChars <|> letter)

bool :: Parser Val
bool = do
  b <- oneOf "tf"
  return $ case b of
    't' -> Bool True
    'f' -> Bool False


quotation :: Parser Val
quotation = do
  char '['
  es <- exprs
  char ']'
  return $ makeWord es



-- API
parseLine =
  parse exprs "forth"

parseFile path =
  parseLine <$> readFile path
