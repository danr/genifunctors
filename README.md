Generate (derive) generalized `fmap`, `foldMap` and `traverse` for `Bifunctors`,
`Trifunctors`, or a functor with any arity

Example:

    data U a b c d
        = L [U a b c d]               -- polymorphic recursion
        | M (V (a,b) (Either c d))    -- mutually recursive
        | a :+: Int                   -- infix syntax, record syntax, type synonyms
        | R { c :: c, d :: String }   -- and primitive data types supported

    data V u v = X (U v v u u) | Z u

    fmapU :: (a -> a') -> (b -> b') -> (c -> c') -> (d -> d') ->
             U a b c d -> U a' b' c' d'
    fmapU = $(genFmap ''U)

    foldU :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> (d -> m) ->
             U a b c d -> m
    foldU = $(genFoldMap ''U)

    travU :: Applicative f =>
             (a -> f a') -> (b -> f b') -> (c -> f c') -> (d -> f d') ->
             U a b c d -> f (U a' b' c' d')
    travU = $(genTraverse ''U)

