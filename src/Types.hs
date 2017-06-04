module Types where

import           Data.IORef

data Method = Method
    deriving (Show)

data Val = NamedList Name [Val]
         | Identifier Name
         | Object { class'     :: Val,
                    attributes :: [(String, Val)] }
         | Class { className       :: Name,
                   instanceMethods :: [(String, Method)] }
    deriving Show



type Name = String
type Item = String
