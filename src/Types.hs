module Types where


data Val = NamedList Name [Val]
         | Identifier Name
    deriving Show


type Name = String
type Item = String
