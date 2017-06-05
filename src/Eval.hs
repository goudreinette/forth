module Eval where

import           Control.Monad.State
import           Types


eval :: Val -> Forth Val
eval val = do
  r <- case val of
    Number n -> do
      push val
      return Nil
    Word (Primitive ".") ->
      pop
    _ -> return Nil
  printStack
  return r





evalMany ::  [Val] -> Forth Val
evalMany vals = last <$> traverse eval vals
