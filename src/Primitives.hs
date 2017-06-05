module Primitives where

import           Types

primitiveWords =
  [("+", numBinOp (+)),
   ("-", numBinOp (-)),
   ("*", numBinOp (*)),
   ("/", numBinOp quot),
   ("max", numBinOp max),
   ("min", numBinOp min),
   ("negate", numOp negate),
   ("abs", numOp abs),
   (".", pop)]

numOp f = do
  (Number x) <- pop
  push (Number (f x))

numBinOp f = do
  (Number x) <- pop
  (Number y) <- pop
  push (Number (f x y))
