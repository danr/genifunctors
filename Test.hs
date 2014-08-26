{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Generics.Genifunctors
import Data.Monoid
import Control.Applicative
import System.Exit

import TestTypes

import Control.Monad.Writer

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

foldTupleRev :: Monoid m => (a -> m) -> (b -> m) -> (a,b) -> m
foldTupleRev f g (a,b) = g b `mappend` f a

foldUCustom :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> (d -> m) -> U a b c d -> m
foldUCustom = $(genFoldMapT [(''(,), 'foldTupleRev)] ''U)

travTupleRev :: Applicative f => (a -> f a') -> (b -> f b') -> (a,b) -> f (a',b')
travTupleRev f g (a,b) = (\b a -> (a,b)) <$> g b <*> f a

travUCustom :: Applicative f => (a -> f a') -> (b -> f b') -> (c -> f c') -> (d -> f d') -> U a b c d -> f (U a' b' c' d')
travUCustom = $(genTraverseT [(''(,), 'travTupleRev), (''V, 'travVCustom)] ''U)

travVCustom :: Applicative f => (a -> f a') -> (b -> f b') -> V a b -> f (V a' b')
travVCustom = $(genTraverseT [(''U, 'travUCustom)] ''V)

assertEq :: (Show a,Eq a) => a -> a -> IO ()
assertEq a b | a == b = return ()
assertEq a b = do
  putStrLn $ show a ++ " /=\n" ++ show b
  exitFailure

main :: IO ()
main = do
    let v = L [0 :+: 1, M (X ((Right 5) :+: 4)), M (Z (2,3)), R 6 ""]
    assertEq (fmapU (+1) (+2) (+3) (+4) v) (L [1 :+: 1, M (X ((Right 9) :+: 4)), M (Z (3,5)), R 9 ""])
    let s = (:[])
    assertEq (foldU s s s s v) ([0,5,2,3,6])
    assertEq (foldUCustom s s s s v) ([0,5,3,2,6])
    let t = tell . s
    assertEq (execWriter (travU t t t t v)) ([0,5,2,3,6])
    assertEq (execWriter (travUCustom t t t t v)) ([0,5,3,2,6])


