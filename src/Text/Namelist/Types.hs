module Text.Namelist.Types where

import Data.Complex
import Data.CaseInsensitive (CI)

data Index
    = Index Int
    | Range Int Int
    | Step  Int Int Int
    deriving (Show, Read, Eq)

data Key
    = Key     (CI String)
    | Indexed (CI String) [Index]
    | Sub     (CI String) Key
    deriving (Show, Read, Eq)

data Value
    = Integer Int
    | Real Double
    | Complex (Complex Double)
    | Logical Bool
    | String String
    | Array [Value]
    | Int :* Value
    | Null
    deriving (Show, Read, Eq)

data Pair = Key := Value
    deriving (Show, Read, Eq)

data Group = Group (CI String) [Pair]
    deriving (Show, Read, Eq)
