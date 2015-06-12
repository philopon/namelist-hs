{-# LANGUAGE DeriveDataTypeable #-}

module Text.Namelist.Types where

import Data.Complex
import Data.CaseInsensitive (CI)
import Data.Data(Data, Typeable)

data Index
    = Index Int
    | Range (Maybe Int) (Maybe Int) (Maybe Int)
    deriving (Show, Read, Data, Typeable, Eq)

data Key
    = Key     (CI String)
    | Indexed (CI String) [Index]
    | Sub     (CI String) Key
    deriving (Show, Read, Data, Typeable, Eq)

data Value
    = Integer Int
    | Real Double
    | Complex (Complex Double)
    | Logical Bool
    | String String
    | Array [Value]
    | Int :* Value
    | Null
    deriving (Show, Read, Data, Typeable, Eq)

data Pair = Key := Value
    deriving (Show, Read, Data, Typeable, Eq)

data Group = Group (CI String) [Pair]
    deriving (Show, Read, Data, Typeable, Eq)
