module Eval where

import           Control.Monad.State
import           Types


eval :: Val -> Forth Val
eval val = do
  state <- get
  case val of
    Number n ->
      push val
    Symbol w ->
      case mode state of
        Compile | w == ";" ->
          dictLookup w >>= invoke . wordType
        Compile ->
          push val
        Interpret ->
          dictLookup w >>= invoke . wordType
    _ -> return Nil


evalMany ::  [Val] -> Forth Val
evalMany vals = last <$> traverse eval vals



-- Invoke
invoke :: WordType -> Forth Val
invoke (Primitive op) = op
invoke (User stack)   = evalMany stack
