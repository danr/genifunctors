{-# LANGUAGE TemplateHaskell #-}
module Test where

import Data.Generics.Genifunctors
import Data.Monoid
import Control.Applicative

data U a b c d
    = A a b (U a b c d)
    | B c d (U a b c d)
    | C a
    | D (U d b c a)
    | E (U (a,b) (b,a) (Either c d) (Either d c))
    | F [U [a] [U b c d a] (Int,String) Double]
    | G (V a b) (V c d) (U (V a b) (V b c) (V c d) (V a d))

data V u v
    = X (U u v u v)
    | Y (U v v u u)
    | Z u

fmapU :: (a -> a') -> (b -> b') -> (c -> c') -> (d -> d') -> U a b c d -> U a' b' c' d'
fmapU = $(genFmap ''U)

foldU :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> (d -> m) -> U a b c d -> m
foldU = $(genFoldMap ''U)

travU :: Applicative f => (a -> f a') -> (b -> f b') -> (c -> f c') -> (d -> f d') -> U a b c d -> f (U a' b' c' d')
travU = $(genTraverse ''U)

