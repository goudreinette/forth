module Types where


data Expr = NamedList Name [Item]
    deriving Show

type Name = String
type Item = String
