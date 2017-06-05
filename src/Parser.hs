module Parser where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Number (int)
import           Types

-- Parsers
exprs :: Parser [Val]
exprs = sepBy expr spaces

expr :: Parser Val
expr = word <|> number

number :: Parser Val
number = Number <$> int

word :: Parser Val
word = do
  w <- oneOf ".+-/*"
  return $ Word (Primitive [w])


-- API
parseLine =
  parse exprs "forth"

parseFile path =
  parseLine <$> readFile path
