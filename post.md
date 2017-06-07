# Writing a Forth

After my Lisp, I'm now working on a stack-based language like Forth.
I enjoy working on 'extreme' languages, because applying a principle everywhere teaches you where the principle makes sense and what the limitations are.
[Extremist Programming](http://blog.ezyang.com/2012/11/extremist-programming/)

In Forth and other concatenative languages, this principle is the stack.
[explanation of stack-based languages]

Forth aims for 'minimal overall complexity', often at the cost of
convenience, compatibility and safety. [Thoughtful Programming](http://www.ultratechnology.com/forththoughts.htm)


Like Lisp, Forth has very little syntax, but powerful metaprogramming capabilities. Comment syntax and basic control flow can actually be defined from inside the language. [My history with Forth & stack machines](http://yosefk.com/blog/my-history-with-forth-stack-machines.html)

## Demo


## Implementation

- Global mutable state
- Monad stack

```haskell
newtype Forth a = Forth
  { unForth :: StateT ForthState IO a }
  deriving (Functor, Applicative, Monad,
            MonadIO, MonadState ForthState)


data ForthState = ForthState
    { interpretStack :: Stack
    , compileStack   :: Stack
    , mode           :: Mode
    , dict           :: Dictionary }
    deriving (Show)


data Mode = Interpret
          | Compile
          deriving (Show)


type Stack =  [Val]
type Dictionary = [(String, Val)]
```

- Dictionary (single enviroment)

```haskell
dictionary =
  [("+", numBinOp (+)),
   ("-", numBinOp (-)),
   ("*", numBinOp (*)),
   ("/", numBinOp quot),
   -- more numeric operations ...
   (".", pop),
   ("dup", dup),
   ("swap", swap),
   (":", compileMode),
   (";", interpretMode),
   ("words", printDict)]
```

- No arguments, pure side effects

```haskell
numBinOp f = do
  (Number x) <- pop
  (Number y) <- pop
  push (Number (f x y))

dup = do
  x <- pop
  push x
  push x
```

- Compile/interpret mode
- interpret:
    - `: square dup * ;`
    -

```haskell
eval :: Val -> Forth Val
eval val = do
  state <- get
  case val of
    Number n ->
      push val
    Symbol w ->
      case mode state of
        Compile | w == ";" ->
          dictLookup w >>= invoke
        Compile ->
          push val
        Interpret ->
          dictLookup w >>= invoke
    _ -> return Nil


invoke :: Val -> Forth Val
invoke f =
  case wordType f of
    Primitive op -> op
    User stack   -> evalMany stack
```

When exiting compile mode, we create a new word from the compile stack, and add it to the dictionary.

```haskell
compileMode :: Forth Val
compileMode = setMode Compile

interpretMode :: Forth Val
interpretMode = do
  (Symbol w:body) <- get <&> stack <&> reverse
  defineWord w (makeWord body)
  modify (setStack [])
  setMode Interpret

setMode :: Mode -> Forth Val
setMode m =
  modifyState $ \state -> state {mode = m}
```

## Conclusion
There's a lot left to explore with concatenative languages.
Control flow is still missing, which requires a separate 'return stack'.
Like Factor, I would like to add Lambda's in too.

The semicolon `;`, used to terminate compile mode, is an example of an `immediate` word.  

Full code on [Github](https://github.com/reinvdwoerd/forth)
