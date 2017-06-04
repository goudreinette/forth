module Types where


data Object = NamedList Name [Object]
            | Identifier Name
    deriving Show


type Name = String
type Item = String
