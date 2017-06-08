module Types where

import           Control.Lens        ((&), (<&>))
import           Control.Monad.State
import           Data.IORef
import           Data.List.Extra

{- State -}
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
run = runStateT . unForth

new :: [(String, Forth Val)] -> ForthState
new bindings =
  ForthState [] [] Interpret dict
  where dict = map wrap bindings
        wrap (s,f) = (s, Word False $ Primitive f)

modifyState :: (ForthState -> ForthState) -> Forth Val
modifyState f = do
  modify f
  return Nil


{- Dictionary -}
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
  d <- get <&> dict
  liftIO (print d)
  return Nil



{- Mode -}
compileMode :: Forth Val
compileMode = setMode Compile

interpretMode :: Forth Val
interpretMode = do
  (Symbol w:body) <- get <&> stack <&> reverse
  defineWord w (makeWord body)
  clearStack
  setMode Interpret

setMode :: Mode -> Forth Val
setMode m =
  modifyState $ \state -> state {mode = m}

showMode :: Mode -> String
showMode Interpret = "i"
showMode Compile   = "c"


{- Stack -}
stack :: ForthState -> Stack
stack state =
  case mode state of
    Interpret -> interpretStack state
    Compile   -> compileStack state

setStack ::Stack -> ForthState -> ForthState
setStack stack state =
  case mode state of
    Interpret -> state {interpretStack = stack}
    Compile   -> state {compileStack = stack}

updateStack :: (Stack -> Stack) -> ForthState -> ForthState
updateStack f state =
  setStack (f $ stack state) state

clearStack :: Forth Val
clearStack = modifyState (setStack [])


push :: Val -> Forth Val
push v = modifyState $ updateStack $ cons v


pop :: Forth Val
pop = do
  state <- get
  case stack state of
    [] ->
      return Nil
    (x:xs) -> do
      modify (setStack xs)
      return x


printStack :: Forth ()
printStack = do
  state <- get
  let m = mode state & showMode
      s = stack state & reverse & map show & unwords
  liftIO $ putStrLn (m ++ "|" ++ s ++ "|")


{- Val -}
data Val = Number Int
         | Bool Bool
         | Symbol String
         | Word { immediate :: Bool, wordType :: WordType }
         | Nil
         deriving (Eq)

data WordType = Primitive (Forth Val)
              | User [Val]


instance Show Val where
  show (Number n)                    = show n
  show (Bool b)                    = if b then "t" else "f"
  show (Symbol s)                    = s
  show Word {wordType = User s}      = showQuotation s
  show Word {wordType = Primitive _} = "<primitive>"
  show Nil                           = ""

instance Eq WordType where
  User s == User s' = s == s'
  _ == _ = False

makeWord :: Stack -> Val
makeWord = Word False . User

showQuotation :: Stack -> String
showQuotation s =
  "[" ++ unwords (map show s) ++ "]"
