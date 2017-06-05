module Primitives where

import           Types

primitiveWords =
  [("+", numOp (+)),
   ("-", numOp (-)),
   (".", pop)]


numOp f = do
  (Number x) <- pop
  (Number y) <- pop
  push (Number (f x y))
