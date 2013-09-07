{-# LANGUAGE TemplateHaskell,DeriveFunctor #-}
module Test where

import Data.Generics.Genifunctors
import Language.Haskell.TH

data T a b = T a b | L a | R b | J Int | Rec (T a b)

bit :: (a -> a') -> (b -> b') -> T a b -> T a' b'
bit = $(genFunctor ''T 'bit)

