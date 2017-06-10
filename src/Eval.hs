module Eval where

import           Control.Monad.State
import           Types


eval :: Val -> Forth ()
eval val = do
  state <- get
  case val of
    Number n ->
      push val
    Bool b ->
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


evalMany ::  [Val] -> Forth ()
evalMany vs = do
  traverse eval vs
  return ()


-- Invoke
invoke :: Val -> Forth ()
invoke f =
  case wordType f of
    Primitive op -> op
    User stack   -> evalMany stack
