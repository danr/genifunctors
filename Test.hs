{-# LANGUAGE TemplateHaskell,DeriveFunctor #-}
module Test where

import Data.Generics.Genifunctors

data T a b = T a b | L a | R b | Rec (T a b) | J Int | K (a,a) | List [(a,T a b)]

data A a = B (B a) | V a
data B b = A (A b) | U b

data P a = P (P (a,a)) | X a

bit :: (a -> a') -> (b -> b') -> T a b -> T a' b'
bit = $(genFunctor ''T)

jit :: (a -> b) -> A a -> A b
jit = $(genFunctor ''A)

pit :: (a -> b) -> P a -> P b
pit = $(genFunctor ''P)

data U a b c d = W a b (U a b c d) | I c d (U a b c d) | Y a | Z (U d b c a)

uit :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1) -> U a b c d -> U a1 b1 c1 d1
uit = $(genFunctor ''U)
