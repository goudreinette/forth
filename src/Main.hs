module Main where

import           Control.Monad.Trans
import           Data.IORef
import           Eval
import           Parser
import           System.Console.Repl
import           Types

evalLine stack x =
  case parseLine x of
    Right v -> evalMany stack v >>= (liftIO . print)
    Left e  -> liftIO (printError e)

main = do
  s <- newIORef []
  repl "=> " (evalLine s)
