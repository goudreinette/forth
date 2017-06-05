module Types where

import           Control.Monad.State
import           Data.IORef
import           Data.List.Extra

-- Forth
newtype Forth a = Forth
  { unForth :: StateT ForthState IO a }
  deriving (Functor, Applicative, Monad,
            MonadIO, MonadState ForthState)

data ForthState = ForthState { stack :: Stack
                             , env   :: Env}

type Stack =  [Val]
type Env = [(String, Val)]


run :: Forth a -> ForthState -> IO (a, ForthState)
run x =
  runStateT (unForth x)

new :: ForthState
new = ForthState [] []


-- Stack
push :: Val -> Forth Val
push v = do
  modify pushV
  return Nil
  where pushV (ForthState xs e) =
          ForthState (v:xs) e


pop :: Forth Val
pop = do
  (ForthState s e) <- get
  case s of
    [] ->
      return Nil
    (x:xs) -> do
      put (ForthState xs e)
      return x

printStack :: Forth ()
printStack = do
  (ForthState s e) <- get
  let str = "|" ++ unwords (map show s) ++ "|"
  liftIO $ putStrLn str


-- Val
data Val = Number Int
         | Word WordType
         | Nil


data WordType = Primitive String
              | User
              deriving Show

instance Show Val where
  show (Number n)           = show n
  show (Word (Primitive w)) = show w
  show (Word (User))        = "<user>"
  show Nil                  = "ok"
