{-# LANGUAGE TemplateHaskell #-}
module Test where

import Data.Generics.Genifunctors
import Data.Monoid
import Control.Applicative

data U a b c d
    = L [U a b c d]               -- polymorphic recursion
    | M (V (a,b) (Either c d))    -- mutually recursive
    | a :+: Int                   -- infix syntax, record syntax, type synonyms
    | R { c :: c, d :: String }   -- and primitive data types supported

data V u v = X (U v v u u) | Z u

fmapU :: (a -> a') -> (b -> b') -> (c -> c') -> (d -> d') -> U a b c d -> U a' b' c' d'
fmapU = $(genFmap ''U)

foldU :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> (d -> m) -> U a b c d -> m
foldU = $(genFoldMap ''U)

travU :: Applicative f => (a -> f a') -> (b -> f b') -> (c -> f c') -> (d -> f d') -> U a b c d -> f (U a' b' c' d')
travU = $(genTraverse ''U)

bimapTuple :: (a -> a') -> (b -> b') -> (a,b) -> (a',b')
bimapTuple = $(genFmap ''(,))

foldMapEither :: Monoid m => (a -> m) -> (b -> m) -> Either a b -> m
foldMapEither = $(genFoldMap ''Either)

travTriple :: Applicative f => (a -> f a') -> (b -> f b') -> (c -> f c') -> (a,b,c) -> f (a',b',c')
travTriple = $(genTraverse ''(,,))
