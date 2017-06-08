module Primitives where

import           Eval
import           Types

dictionary =
  [("+", numBinOp (+)),
   ("-", numBinOp (-)),
   ("*", numBinOp (*)),
   ("/", numBinOp quot),
   ("max", numBinOp max),
   ("min", numBinOp min),
   ("negate", numOp negate),
   ("abs", numOp abs),

   (".", pop),
   ("dup", dup),
   ("swap", swap),
   ("call", call),

   (":", compileMode),
   (";", interpretMode),

   ("words", printDict)]


-- Quotations
call = do
  (Word _ (User stack)) <- pop
  evalBody stack

-- Helpers
numOp f = do
  (Number x) <- pop
  push (Number (f x))

numBinOp f = do
  (Number x) <- pop
  (Number y) <- pop
  push (Number (f x y))


-- Stack Manipulation
dup = do
  x <- pop
  push x
  push x

swap = do
  x <- pop
  y <- pop
  push x
  push y
