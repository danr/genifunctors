{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
module TestTypes where

data U a b c d
    = L [U a b c d]               -- polymorphic recursion
    | M (V (a,b) (Either c d))    -- mutually recursive
    | a :+: Int                   -- infix syntax, record syntax, type synonyms
    | R { c :: c, d :: String }   -- and primitive data types supported
 deriving (Eq,Show)

data V u v = X (U v v u u) | Z u
 deriving (Eq,Show)

data W (a :: *) b = W b
 deriving (Eq,Show)
