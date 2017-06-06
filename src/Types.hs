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


-- Env
dictLookup :: String -> Forth Val
dictLookup w = do
  e <- dict <$> get
  case lookup w e of
    Just w ->
      return w
    Nothing ->
      error ("Lookup failed: " ++ w)

defineWord :: String -> Val -> Forth ()
defineWord s v = do
  modifyState addBinding
  where addBinding = cons


-- Compiling
compileMode :: Forth Val
compileMode = setMode Compile

interpretMode :: Forth Val
interpretMode = do
  s <- stack <$> get

  setMode Interpret

setMode :: Mode -> Forth Val
setMode m =
  modifyState $ \state -> state {mode = m}




-- Stack
stack :: ForthState -> Stack
stack state =
  case mode state of
    Interpret -> interpretStack state
    Compile   -> compileStack state

setStack :: ForthState -> Stack -> ForthState
setStack state stack =
  case mode state of
    Interpret -> state {interpretStack = stack}
    Compile   -> state {compileStack = stack}

updateStack :: (Stack -> Stack) -> ForthState -> ForthState
updateStack f state =
  setStack state $ f $ stack state


push :: Val -> Forth Val
push v = modifyState (updateStack (cons v))


pop :: Forth Val
pop = do
  state <- get
  case stack state of
    [] ->
      return Nil
    (x:xs) -> do
      put (setStack state xs)
      return x


printStack :: Forth ()
printStack = do
  s <- stack <$> get
  let str = "|" ++ unwords (map show (reverse s)) ++ "|"
  liftIO $ putStrLn str


-- Val
data Val = Number Int
         | Word String
         | Primitive (Forth Val)
         | User [Val]
         | Nil



instance Show Val where
  show (Number n)    = show n
  show (Word s)      = s
  show (Primitive _) = "<primitive>"
  show Nil           = "ok"
