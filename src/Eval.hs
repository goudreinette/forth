module Eval where

import           Control.Monad.State
import           Types


eval :: Val -> Forth Val
eval val = do
  state <- get
  case val of
    Number n ->
      push val
    Word _ _ ->
      push val
    Symbol w ->
      case mode state of
        Compile | w == ";" ->
          dictLookup w >>= invoke
        Compile ->
          push val
        Interpret ->
          dictLookup w >>= invoke
    _ -> return Nil


evalMany ::  [Val] -> Forth [Val]
evalMany = traverse eval

evalBody ::  [Val] -> Forth Val
evalBody body = last <$> evalMany body


-- Invoke
invoke :: Val -> Forth Val
invoke f =
  case wordType f of
    Primitive op -> op
    User stack   -> evalBody stack
