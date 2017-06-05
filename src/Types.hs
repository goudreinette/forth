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

new :: [(String, Forth Val)] -> ForthState
new bindings =
  ForthState [] env
  where env = map wrap bindings
        wrap (s,f) = (s, Primitive f)

envLookup :: String -> Forth Val
envLookup w = do
  (ForthState s e) <- get
  case lookup w e of
    Just w ->
      return w
    Nothing ->
      undefined


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
         | Word String
         | Primitive (Forth Val)
         | Nil



instance Show Val where
  show (Number n)    = show n
  show (Word s)      = s
  show (Primitive _) = "<primitive>"
  show Nil           = "ok"
