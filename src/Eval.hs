module Eval where

import           Control.Monad.State
import           Types


eval :: Val -> Forth Val
eval val = do
  r <- case val of
    Number n ->
      push val
    Symbol w -> do
      s <- get
      case mode s of
        Compile | w == ";" ->
          dictLookup w >>= invoke . wordType
        Compile ->
          push (Symbol w)
        Interpret ->
          dictLookup w >>= invoke . wordType
    _ -> return Nil
  printStack
  return r


evalMany ::  [Val] -> Forth Val
evalMany vals = last <$> traverse eval vals



-- Invoke
invoke :: WordType -> Forth Val
invoke (Primitive op) = op
invoke (User stack)   = evalMany stack
