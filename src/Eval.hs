module Eval where

import           Control.Monad.State
import           Types


eval :: Val -> Forth Val
eval val = do
  r <- case val of
    Number n ->
      push val
    Word w -> do
      (Primitive op) <- envLookup w
      op
    _ -> return Nil
  printStack
  return r





evalMany ::  [Val] -> Forth Val
evalMany vals = last <$> traverse eval vals
