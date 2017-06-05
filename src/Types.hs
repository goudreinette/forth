module Types where

import           Data.IORef
import           Data.List.Extra

-- Forth
data Forth = Forth { stack :: Stack
                   , env   :: Env}


-- Stack
type Stack = IORef [Val]
type Env = IORef [(String, Val)]

push :: Stack -> Val -> IO ()
push stack v =
  modifyIORef stack (cons v)

pop :: Stack -> IO Val
pop stack = do
  s <- readIORef stack
  case s of
    [] ->
      return Nil
    (x:xs) -> do
      writeIORef stack xs
      return x

printStack :: Stack -> IO ()
printStack stack = do
  s <- readIORef stack
  print s

new = do
  s <- newIORef []
  e <- newIORef []
  return (Forth s e)


-- Val
data Val = Number Int
         | Word WordType
         | Nil
         deriving Show


data WordType = Primitive String
              | User
              deriving Show
