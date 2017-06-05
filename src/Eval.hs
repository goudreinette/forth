module Eval where

import           Control.Monad.State
import           Types


eval :: Stack -> Val -> IO Val
eval stack val = do
  r <- case val of
    Number n -> do
      push stack val
      return Nil
    Word (Primitive ".") ->
      pop stack
    _ -> return Nil
  printStack stack
  return r





evalMany :: Stack -> [Val] -> IO Val
evalMany stack vals = last <$> traverse (eval stack) vals
