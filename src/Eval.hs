module Eval where

import           Control.Monad.State
import           Types


eval :: Val -> Forth Val
eval val = do
  r <- case val of
    Number n ->
      push val
    Word w -> do
      s <- get
      case mode s of
        Compile | w == ";" ->
          dictLookup w >>= invoke
        Compile ->
          push (Word w)
        Interpret ->
          dictLookup w >>= invoke
    _ -> return Nil
  printStack
  return r


evalMany ::  [Val] -> Forth Val
evalMany vals = last <$> traverse eval vals



-- Invoke
invoke :: Val -> Forth Val
invoke (Primitive op) = op
invoke (User stack)   = evalMany stack
invoke x              = error ("not a valid word: " ++ show x)
