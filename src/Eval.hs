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
        Compile | w == ";" -> do
          (Primitive op) <- dictLookup w
          op
        Compile ->
          push (Word w)
        Interpret -> do
          (Primitive op) <- dictLookup w
          op
    _ -> return Nil
  printStack
  return r





evalMany ::  [Val] -> Forth Val
evalMany vals = last <$> traverse eval vals
