{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
#endif
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

#if __GLASGOW_HASKELL__ >= 708
data family Sing (a :: k)
data instance Sing (a :: Bool) where
    SFalse :: Sing False
    STrue  :: Sing True

data ZB a b (τ :: Bool) where
  ZT :: a -> ZB a b True
  ZF :: b -> ZB a b False
  ZR :: ZB a b τ -> ZB a b τ

data Z a (τ :: Bool) b where
  A1 :: a       -> Z a False b
  A2 :: (a,a)   -> Z a True  b 
  A3 :: b       -> Z a False b
  AB :: ZB a String τ -> Z b τ a
  AN :: Sing τ -> [a] -> Z a τ c
  AS :: Z a True b -> Z a True b
  AF :: Z a True x -> Z a True b

data ZU f a where
  ZU :: f Int -> a -> ZU f a
#endif
