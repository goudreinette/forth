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
                             , dict  :: Dictionary
                             , mode  :: Mode }
                             deriving (Show)

data Mode = Interpreting
          | Compiling Stack
          deriving (Show)


type Stack =  [Val]
type Dictionary = [(String, Val)]


run :: Forth a -> ForthState -> IO (a, ForthState)
run x =
  runStateT (unForth x)

new :: [(String, Forth Val)] -> ForthState
new bindings =
  ForthState [] dict Interpreting
  where dict = map wrap bindings
        wrap (s,f) = (s, Primitive f)

-- Env
dictLookup :: String -> Forth Val
dictLookup w = do
  e <- dict <$> get
  case lookup w e of
    Just w ->
      return w
    Nothing ->
      undefined


-- Compiling
compileMode :: Forth Val
compileMode = setMode (Compiling [])

interpretMode :: Forth Val
interpretMode = setMode Interpreting

setMode :: Mode -> Forth Val
setMode m = do
  modify $ \state -> state {mode = m}
  get >>= liftIO . print
  return Nil

compilePush :: Val -> Forth Val
compilePush v = do
  modify pushV
  return Nil
  where pushV state@ForthState {mode = (Compiling xs)} =
          state { mode = Compiling (v:xs)}
        pushV state =
          state



-- Stack
push :: Val -> Forth Val
push v = do
  modify pushV
  return Nil
  where pushV state@ForthState {stack = xs} =
          state {stack = v:xs}


pop :: Forth Val
pop = do
  state <- get
  case stack state of
    [] ->
      return Nil
    (x:xs) -> do
      put (state {stack = xs})
      return x

printStack :: Forth ()
printStack = do
  s <- stack <$> get
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
