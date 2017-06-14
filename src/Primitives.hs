module Primitives where

import           Control.Monad.Trans
import           Eval
import           System.Process
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

   ("=", equal),
   ("if", if'),

   (".", pop'),
   (".s", printStack),
   ("dup", dup),
   ("swap", swap),
   ("call", call),
   ("compose", compose),
   ("curry", curry'),

   (":", compileMode),
   (";", interpretMode),

   ("words", printDict),

   ("sh", sh)]


-- Quotations
call = do
  q <- pop
  invoke q

compose = do
  y <- pop
  x <- pop
  push $ makeWord (wordBody x ++ wordBody y)

curry' = do
  q <- pop
  x <- pop
  push $ makeWord (x : wordBody q)


-- Helpers
numOp f = do
  (Number x) <- pop
  push (Number (f x))

numBinOp f = do
  (Number x) <- pop
  (Number y) <- pop
  push (Number (f x y))


-- Equality
equal = do
  x <- pop
  y <- pop
  push (Bool (x == y))

if' = do
  y <- pop
  x <- pop
  b <- pop
  if b == Bool False then
    invoke y
  else
    invoke x


-- Stack Manipulation
pop' = do
  v <- pop
  printVal v

dup = do
  x <- pop
  push x
  push x

swap = do
  x <- pop
  y <- pop
  push x
  push y

-- IO
sh = do
  (String x) <-pop
  let (cmd:args) = words x
  result <- liftIO $ readProcess cmd args ""
  push $ String result
