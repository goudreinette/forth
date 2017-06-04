module Parse where

import           Text.Parsec.Indent
import           Text.Parsec.Prim              (runParserT)
import           Text.ParserCombinators.Parsec hiding (Parser, parse)

type Parser a = IndentParser String () a

inputText = unlines [
        "listName:",
        "  item1",
        "  item2",
        "  item3"
    ]


parse :: Parser a -> SourceName -> String -> Either ParseError a
parse aParser source_name input =
  runIndent $ runParserT aParser () source_name input
