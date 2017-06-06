module Types where

import           Control.Monad.State
import           Data.IORef
import           Data.List.Extra

-- Forth
newtype Forth a = Forth
  { unForth :: StateT ForthState IO a }
  deriving (Functor, Applicative, Monad,
            MonadIO, MonadState ForthState)

data ForthState = ForthState { interpretStack :: Stack
                             , compileStack   :: Stack
                             , mode           :: Mode
                             , dict           :: Dictionary }
                             deriving (Show)

data Mode = Interpret
          | Compile
          deriving (Show)


type Stack =  [Val]
type Dictionary = [(String, Val)]


run :: Forth a -> ForthState -> IO (a, ForthState)
run x =
  runStateT (unForth x)

new :: [(String, Forth Val)] -> ForthState
new bindings =
  ForthState [] [] Interpret dict
  where dict = map wrap bindings
        wrap (s,f) = (s, Primitive f)

modifyState :: (ForthState -> ForthState) -> Forth Val
modifyState f = do
  modify f
  return Nil


currentStack :: ForthState -> Stack
currentStack state =
  case mode state of
    Interpret -> interpretStack state
    Compile   -> compileStack state

setCurrentStack :: ForthState -> Stack -> ForthState
setCurrentStack state stack =
  case mode state of
    Interpret -> state {interpretStack = stack}
    Compile   -> state {compileStack = stack}

updateCurrentStack :: (Stack -> Stack) -> ForthState -> ForthState
updateCurrentStack f state =
  setCurrentStack state $ f $ currentStack state


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
compileMode = setMode Compile

interpretMode :: Forth Val
interpretMode = setMode Interpret

setMode :: Mode -> Forth Val
setMode m = do
  modify $ \state -> state {mode = m}
  get >>= liftIO . print
  return Nil





-- Stack
push :: Val -> Forth Val
push v = modifyState (updateCurrentStack (cons v))


pop :: Forth Val
pop = do
  state <- get
  case currentStack state of
    [] ->
      return Nil
    (x:xs) -> do
      put (setCurrentStack state xs)
      return x


printStack :: Forth ()
printStack = do
  s <- currentStack <$> get
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
