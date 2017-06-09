# Writing a Forth: Quotations


Concatenation is composition
Transparent printing and equality
Partial application

```forth
[1 1 =] .
\ [1 1 =] ok
4 [>] curry .
\ [4 >] ok
[1 +] [2 *] compose .
\ [1 + 2 *] ok
5 [1 + 2 *] call .
\ 12 ok
```

## Types
```haskell
data Val = Number Int
         | Bool Bool
         | Symbol String
         | Word { immediate :: Bool, wordType :: WordType }
         | Nil
         deriving (Eq)

data WordType = Primitive (Forth Val)
              | User [Val]
```

## Printing

```haskell
instance Show Val where
  -- ...
  show Word {wordType = User s} =
      showQuotation s

showQuotation :: Stack -> String
showQuotation s =
  "[" ++ unwords (map show s) ++ "]"
```

## Equality

```haskell
instance Eq WordType where
  User s == User z = s == z
  _ == _ = False
```

## Parsing
```haskell
exprs :: Parser [Val]
exprs = many expr

expr :: Parser Val
expr =
  between spaces spaces $ bool <|> word <|> number <|> quotation

quotation :: Parser Val
quotation = do
  char '['
  es <- exprs
  char ']'
  return $ makeWord es
```


## Eval
like other values: pushed, not invoked
```haskell
eval :: Val -> Forth Val
eval val = do
  state <- get
  case val of
    -- ...
    Word _ _ ->
      push val
    -- ...
```

## Call
```haskell
call = do
  q <- pop
  evalBody (wordBody q)
```


## Compose
```haskell
compose = do
  y <- pop
  x <- pop
  push $ makeWord (wordBody x ++ wordBody y)
```


## Curry


```haskell
curry' = do
  q <- pop
  x <- pop
  push $ makeWord (x : wordBody q)
```
