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


-- Dictionary
dictLookup :: String -> Forth Val
dictLookup w = do
  e <- dict <$> get
  case lookup w e of
    Just w ->
      return w
    Nothing ->
      error ("Lookup failed: " ++ w)

defineWord :: String -> Val -> Forth Val
defineWord s v =
  modifyState $ \state@ForthState {dict = d} ->
    state {dict = (s, v):d}

printDict :: Forth Val
printDict = do
  d <- dict <$> get
  liftIO (print d)
  return Nil




-- Mode
compileMode :: Forth Val
compileMode = setMode Compile

interpretMode :: Forth Val
interpretMode = do
  (Symbol w:body) <- reverse . stack <$> get
  defineWord w (User body)
  modify (`setStack` [])
  setMode Interpret

setMode :: Mode -> Forth Val
setMode m =
  modifyState $ \state -> state {mode = m}

showMode :: Mode -> String
showMode Interpret = "i"
showMode Compile   = "c"


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
  state <- get
  let str =  showMode (mode state) ++ "|" ++ unwords (map show (reverse (stack state))) ++ "|"
  liftIO $ putStrLn str


-- Val
data Val = Number Int
         | Symbol String
         | Primitive (Forth Val)
         | User [Val]
         | Nil



instance Show Val where
  show (Number n)    = show n
  show (Symbol s)      = s
  show (User _)      = "<user>"
  show (Primitive _) = "<primitive>"
  show Nil           = "ok"
