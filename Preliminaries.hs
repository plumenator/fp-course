module Preliminaries where

import Prelude

data List a = Empty | a ::: List a deriving (Show)

infixr 5 :::

isEmpty :: List a -> Bool
isEmpty Empty = True
isEmpty _ = False

mkSingleton :: a -> List a
mkSingleton x = x ::: Empty

class ToString a where
  toString :: a -> String

instance Show a => ToString (List a) where
  toString = show

data MyBool = MyTrue | MyFalse deriving (Show)

instance ToString MyBool where
  toString = show

class Falsity a where
  isFalse :: a -> Bool
  isTrue :: a -> Bool
  isTrue x = not (isFalse x)

instance Falsity MyBool where
  isFalse MyFalse = True
  isFalse MyTrue = False

instance Falsity (List a) where
  isFalse = isEmpty

instance Falsity Int where
  isFalse n | n <= 0  = True
  isFalse _ = False
