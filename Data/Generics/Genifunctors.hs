{-# LANGUAGE CPP,TemplateHaskell,PatternGuards,RecordWildCards #-}
-- | Generate (derive) generalized 'fmap', 'foldMap' and 'traverse' for Bifunctors, Trifunctors, or a functor with any arity
--
-- Example:
--
-- @
--data U a b c d
--    = L [U a b c d]               -- polymorphic recursion
--    | M (V (a,b) (Either c d))    -- mutually recursive
--    | a :+: Int                   -- infix syntax, record syntax, type synonyms
--    | R { c :: c, d :: String }   -- and primitive data types supported
--
--data V u v = X (U v v u u) | Z u
--
--fmapU :: (a -> a') -> (b -> b') -> (c -> c') -> (d -> d') -> U a b c d -> U a' b' c' d'
--fmapU = $(genFmap ''U)
--
--foldU :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> (d -> m) -> U a b c d -> m
--foldU = $(genFoldMap ''U)
--
--travU :: Applicative f => (a -> f a') -> (b -> f b') -> (c -> f c') -> (d -> f d') -> U a b c d -> f (U a' b' c' d')
--travU = $(genTraverse ''U)
-- @
--
-- 'genFoldMapT' and 'genTraverseT' allow for specifying custom functions to handle
-- subparts of a specific type. The compiler will throw an error if any of the
-- types is actually a type synonym.
module Data.Generics.Genifunctors
  ( genFmap
  , genFoldMap
  , genFoldMapT
  , genTraverse
  , genTraverseT
  ) where

import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import Control.Monad.RWS
import Data.Map (Map)
import qualified Data.Map as M

import Control.Exception(assert)
import Data.Maybe
import Data.Either
import Data.List
import Data.Char

type GenM = RWST Generator [Dec] (Map Name Name) Q

data Generator = Generator
    { gen_combine    :: Name -> [Exp] -> Exp
    , gen_primitive  :: Exp -> Exp
    , gen_type       :: Name -> [Either TyVarBndr Name] -> Q Type
    , gen_type_inner :: Name -> [Either TyVarBndr Name] -> Q Type
    }

gen :: Generator -> Name -> Q Exp
gen g = genT g []
        
genT :: Generator -> [(Name,Name)] -> Name -> Q Exp
genT generator predef tc = do
    forM_ predef $ \(con,_) -> do
      info <- reify con
      case info of
        TyConI (TySynD{}) -> fail $ show con ++ " is a type synonym."
        TyConI _          -> return ()
        _                 -> fail $ show con ++ " is not a type constructor." 
    (fn,decls) <- evalRWST (generate tc) generator $ M.fromList predef
    return $ LetE decls (VarE fn)

-- | Generate generalized 'fmap' for a type
--
-- @
--bimapTuple :: (a -> a') -> (b -> b') -> (a,b) -> (a',b')
--bimapTuple = $(genFmap ''(,))
-- @
genFmap :: Name -> Q Exp
genFmap = gen Generator
    { gen_combine   = fmapCombine
    , gen_primitive = fmapPrimitive
    , gen_type      = fmapType
    , gen_type_inner = fmapTypeInner
    }

-- | Generate generalized 'foldMap' for a type
--
-- @
--foldMapEither :: Monoid m => (a -> m) -> (b -> m) -> Either a b -> m
--foldMapEither = $(genFoldMap ''Either)
-- @
genFoldMap :: Name -> Q Exp
genFoldMap = genFoldMapT []

-- | Generate generalized 'foldMap' for a type, optionally traversing
--   subparts of it with custom implementations.
--
-- @
--foldTupleRev :: Monoid m => (a -> m) -> (b -> m) -> (a,b) -> m
--foldTupleRev f g (a,b) = g b <> f a
--
--foldUCustom :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> (d -> m) -> U a b c d -> m
--foldUCustom = $(genFoldMapT [(''(,), 'foldTupleRev)] ''U)
-- @
genFoldMapT :: [(Name,Name)] -> Name -> Q Exp
genFoldMapT = do
  genT Generator
    { gen_combine   = foldMapCombine
    , gen_primitive = foldMapPrimitive
    , gen_type      = foldMapType
    , gen_type_inner = foldMapTypeInner
    }

-- | Generate generalized 'traversable' for a type
--
-- @
--travTriple :: Applicative f => (a -> f a') -> (b -> f b') -> (c -> f c') -> (a,b,c) -> f (a',b',c')
--travTriple = $(genTraverse ''(,,))
-- @
genTraverse :: Name -> Q Exp
genTraverse = genTraverseT []

-- | Generate generalized 'traversable' for a type, optionally traversing
--   subparts of it with custom implementations.
--
-- @
--travTupleRev :: Applicative f => (a -> f a') -> (b -> f b') -> (a,b) -> f (a',b')
--travTupleRev f g (a,b) = (\b a -> (a,b)) <$> g b <*> f a
--
--travUCustom :: Applicative f => (a -> f a') -> (b -> f b') -> (c -> f c') -> (d -> f d') -> U a b c d -> f (U a' b' c' d')
--travUCustom = $(genTraverseT [(''(,), 'travTupleRev), (''V, 'travVCustom)] ''U)
--
--travVCustom :: Applicative f => (a -> f a') -> (b -> f b') -> V a b -> f (V a' b')
--travVCustom = $(genTraverseT [(''U, 'travUCustom)] ''V)
-- @
genTraverseT :: [(Name, Name)] -> Name -> Q Exp
genTraverseT = genT Generator
    { gen_combine   = traverseCombine
    , gen_primitive = traversePrimitive
    , gen_type      = traverseType ''Applicative
    , gen_type_inner = traverseTypeInner ''Applicative
    }

fmapCombine :: Name -> [Exp] -> Exp
fmapCombine con_name args = foldl AppE (ConE con_name) args

foldMapCombine :: Name -> [Exp] -> Exp
foldMapCombine _con_name [] = VarE 'mempty
foldMapCombine _con_name as = foldr1 (<<>>) as

traverseCombine :: Name -> [Exp] -> Exp
traverseCombine con_name []     = VarE 'pure `AppE` ConE con_name
traverseCombine con_name (a:as) = foldl (<***>) (ConE con_name <$$> a) as

mkInfix :: Name -> Exp -> Exp -> Exp
mkInfix n e1 e2 = InfixE (Just e1) (VarE n) (Just e2)

(<***>) :: Exp -> Exp -> Exp
(<***>) = mkInfix '(<*>)

(<$$>) :: Exp -> Exp -> Exp
(<$$>) = mkInfix '(<$>)

(<<>>) :: Exp -> Exp -> Exp
(<<>>) = mkInfix 'mappend

fmapPrimitive :: Exp -> Exp
fmapPrimitive = id

foldMapPrimitive :: Exp -> Exp
foldMapPrimitive = const (VarE 'mempty)

traversePrimitive :: Exp -> Exp
traversePrimitive e = VarE 'pure `AppE` e

rfrKinded :: Either TyVarBndr Name -> Q (Either TyVarBndr Name)
rfrKinded v = case v of
  Left x -> fmap Left$ case x of
    (PlainTV a) -> PlainTV `liftM` newName (nameBase a)
    (KindedTV a k) -> KindedTV `liftM` newName (nameBase a) `ap` return k
  Right y -> return$ Right y

rfrPlain :: Either TyVarBndr Name -> Q (Either TyVarBndr Name)
rfrPlain v = case v of
  Left x -> return$ Left x
  Right y -> fmap Right$ newName $ nameBase y

varName :: Either TyVarBndr Name -> Name
varName t = case t of
  Right x -> x
  Left  x -> tyVarName x 

tyVarName (PlainTV a) = a
tyVarName (KindedTV a _) = a
                           
fmapType :: Name -> [Either TyVarBndr Name] -> Q Type
fmapType tc tvs' = do
    tvs  <- mapM rfrKinded tvs'
    from <- mapM rfrPlain tvs
    to   <- mapM rfrPlain tvs
    return $ ForallT (lefts from ++ (map PlainTV $ rights (from ++ to))) []
           $ foldr arr
                (applyTyVars tc (map varName from) `arr` applyTyVars tc (map varName to))
                (zipWith arr (map VarT $ rights from) (map VarT $ rights to))

fmapTypeInner :: Name -> [Either TyVarBndr Name] -> Q Type
fmapTypeInner tc tvs' = do
    tvs  <- mapM rfrKinded tvs'

    ignore <- mapM rfrKinded tvs'

    from <- mapM rfrPlain tvs
    to   <- mapM rfrPlain tvs

    return $ ForallT (lefts from ++ (map PlainTV $ rights (from ++ to)) ++ [PlainTV (tyVarName i) | i <- lefts ignore]) []
           $ foldr arr
                (applyTyVars tc (map varName from) `arr` applyTyVars tc (map varName to))
                [ case (i,a,b) of
                     (Right _, Right a, Right b) -> VarT a `arr` VarT b
                     (Left  v, _      , _      ) -> VarT (tyVarName v) `arr` VarT (tyVarName v)
                | (i, a, b) <- zip3 ignore from to ]

foldMapType :: Name -> [Either TyVarBndr Name] -> Q Type
foldMapType tc tvs' = do
    m <- newName "m"
    tvs  <- mapM rfrKinded tvs'
    from <- mapM rfrPlain tvs
    return $ ForallT (lefts from ++ map PlainTV (m : rights from))
#if MIN_VERSION_template_haskell(2,10,0)
      [AppT (ConT ''Monoid) (VarT m)]
#else
      [ClassP ''Monoid [VarT m]]
#endif

           $ foldr arr
                (applyTyVars tc (map varName from) `arr` VarT m)
                (zipWith arr (map VarT$ rights from) (repeat (VarT m)))


foldMapTypeInner :: Name -> [Either TyVarBndr Name] -> Q Type
foldMapTypeInner tc tvs' = do
    m <- newName "m"
    tvs  <- mapM rfrKinded tvs'
    ignore <- mapM rfrKinded tvs'
    from <- mapM rfrPlain tvs
    return $ ForallT (lefts from ++ map PlainTV (m : rights from) ++ [PlainTV (tyVarName i) | i <- lefts ignore])
#if MIN_VERSION_template_haskell(2,10,0)
      [AppT (ConT ''Monoid) (VarT m)]
#else
      [ClassP ''Monoid [VarT m]]
#endif
           $ foldr arr
                (applyTyVars tc (map varName from) `arr` VarT m)
                [ case (i,a) of
                     (Right _, Right a) -> VarT a `arr` VarT m
                     (Left  v, _      ) -> VarT (tyVarName v) `arr` VarT m
                | (i, a) <- zip ignore from ]

traverseType :: Name -> Name -> [Either TyVarBndr Name] -> Q Type
traverseType constraint_class tc tvs' = do
    f <- newName "f"
    tvs  <- mapM rfrKinded tvs'
    from <- mapM rfrPlain tvs
    to   <- mapM rfrPlain tvs
    return $ ForallT (lefts from ++ (map PlainTV (f: rights (from ++ to))))
#if MIN_VERSION_template_haskell(2,10,0)
      [AppT (ConT constraint_class) (VarT f)]
#else
      [ClassP constraint_class [VarT f]]
#endif
           $ foldr arr
                ((applyTyVars tc (map varName from)) `arr` (VarT f `AppT` applyTyVars tc (map varName to)))
                (zipWith arr (map VarT $ rights from) (map (\ t -> VarT f `AppT` VarT t) $ rights to))

traverseTypeInner :: Name -> Name -> [Either TyVarBndr Name] -> Q Type
traverseTypeInner constraint_class tc tvs' = do
    f <- newName "f"
    tvs  <- mapM rfrKinded tvs'
    ignore <- mapM rfrKinded tvs'
    from <- mapM rfrPlain tvs
    to   <- mapM rfrPlain tvs
    return $ ForallT (lefts from ++ (map PlainTV (f: rights (from ++ to))) ++ [PlainTV (tyVarName i) | i <- lefts ignore])
#if MIN_VERSION_template_haskell(2,10,0)
      [AppT (ConT constraint_class) (VarT f)]
#else
      [ClassP constraint_class [VarT f]]
#endif
           $ foldr arr
                ((applyTyVars tc (map varName from)) `arr` (VarT f `AppT` applyTyVars tc (map varName to)))
                [ case (i,a,b) of
                     (Right _, Right a, Right b) -> VarT a `arr` (VarT f `AppT` VarT b)
                     (Left  v, _      , _      ) -> VarT (tyVarName v) `arr` (VarT f `AppT` VarT (tyVarName v))
                | (i, a, b) <- zip3 ignore from to ]

-- Type has kind *
genMatch :: Type -> [(Name, (Either TyVarBndr Name, Name))] -> GenM Exp
genMatch t tvfs = do
    Generator{..} <- ask
    let
        tvn  = [n | (_,(Right n, v)) <- tvfs]

        -- | Avoid generating traversals for types that don't mention
        --   any of the given variables.
        --   This both reduce code size, and avoids crashing on some
        --   incompatible type constructors (e.g. family instances)
        go0 :: Type -> GenM Exp
        go0 t =
          let free = freeVarsType t in
          if any (`elem` tvn) free then go t
          else genPrimitive

        go :: Type -> GenM Exp
        go t = case t of
          VarT a     | Just (_,f) <- lookup a tvfs -> return$ VarE f
          AppT t1 t2 -> AppE <$> go t1 <*> go0 t2
          ConT tc    -> VarE <$> generate' tc
          TupleT i   -> VarE <$> generate' (tupleTypeName i)
          ListT      -> VarE <$> generate' ''[]
          _          -> error $ "genMatch:go:" ++ show t

    go0 t

genPrimitive :: GenM Exp
genPrimitive = do
  Generator{..} <- ask
  x <- q $ newName "x"
  return (LamE [VarP x] (gen_primitive $ VarE x))

freeVarsType :: Type -> [Name]
freeVarsType ty = go ty
  where
    go ty = case ty of
      ForallT tyv _ ty  -> go ty \\ map tyVarName tyv
      VarT n                      -> [n]
      AppT t1 t2                  -> go t1 ++ go t2
      SigT t _                    -> go t
      ConT _                      -> []
      TupleT _                    -> []
      ArrowT                      -> []
      ListT                       -> []
#if __GLASGOW_HASKELL__ >= 704
      UnboxedTupleT _             -> []
#endif
#if __GLASGOW_HASKELL__ >= 706
      StarT                       -> []
      ConstraintT                 -> []
      LitT _                      -> []
      PromotedT _                 -> []
      PromotedNilT                -> []
      PromotedConsT               -> []
      PromotedTupleT _            -> []
#endif

simpCon :: Con -> (Name,[Type])
simpCon con = case con of
    NormalC n ts   -> (n,map snd ts)
    RecC n vts     -> (n,map (\ (_,_,t) -> t) vts)
    InfixC t1 n t2 -> (n,[snd t1,snd t2])
    ForallC _ _ con -> simpCon con

generate :: Name -> GenM Name
generate tc = do
  Generator{..} <- ask

  fn0 <- generate' tc
  fn <- q $ newSanitizedName ("_" ++ nameBase tc)
  (tvs,_) <- getTyConInfo tc
  ty <- q $ gen_type tc tvs
  fs <- zipWithM (const . q . newName) (repeat "_f") tvs
  primitive <- genPrimitive
  tell
    [ SigD fn ty
    , FunD fn [ Clause (map VarP [ f | (f,Right _) <- zip fs tvs ])
                       (NormalB (foldl AppE (VarE fn0) [ case v of
                                                            Right _ -> VarE f
                                                            Left  _ -> primitive
                                                 | (f,v) <- zip fs tvs ]))
                       []]]
  return fn

generate' :: Name -> GenM Name
generate' tc = do
    m_fn <- gets (M.lookup tc)
    case m_fn of
        Just fn -> return fn
        Nothing -> do

            Generator{..} <- ask

            fn <- q $ newSanitizedName (nameBase tc)
            modify (M.insert tc fn)
            (tvs,cons) <- getTyConInfo tc
            fs <- zipWithM (const . q . newName) (repeat "_f") tvs
            x <- q $ newName "_x"

            body <- if null cons || null tvs
                then return $ gen_primitive (VarE x)  -- primitive data types/non polymorphic
                else do
                    matches <- forM (map simpCon cons) $ \ (con_name,ts) -> do

                        ys <- zipWithM (const . q . newName) (repeat "_y") ts

                        lhs <- gen_combine con_name <$> sequence
                                [ do t' <- q (expandSyn t)
                                     le <- genMatch t' [(varName v, (v,f)) | (v, f) <- zip tvs fs]
                                     return (le `AppE` VarE y)
                                | (y,t) <- zip ys ts ]

                        return $ Match (ConP con_name (map VarP ys)) (NormalB lhs) []

                    return (CaseE (VarE x) matches)

            ty <- q $ gen_type_inner tc tvs

            tell
                [ SigD fn ty
                , FunD fn [ Clause (map VarP (fs ++ [x])) (NormalB $ body) [] ]
                ]
            return fn

newSanitizedName :: String -> Q Name
newSanitizedName nb = newName $ '_':'N':(
    nb >>= \x -> case x of
      c | isAlpha c || isDigit c -> c:[]
      '\'' -> "\'"
      '_'  -> "__"
      c    -> "_" ++ show (ord c))

arr :: Type -> Type -> Type
arr t1 t2 = (ArrowT `AppT` t1) `AppT` t2

applyTyVars :: Name -> [Name] -> Type
applyTyVars tc ns = foldl AppT (ConT tc) (map VarT ns)

q :: Q a -> GenM a
q = lift

-- All the following functions are by Lennart in Geniplate

getTyConInfo :: Name -> GenM ([Either TyVarBndr Name], [Con])
getTyConInfo con = do
    info <- q (reify con)
    case info of
        TyConI (DataD _ _ tvs cs _) -> return (map unPlainTv tvs, cs)
        TyConI (NewtypeD _ _ tvs c _) -> return (map unPlainTv tvs, [c])
        PrimTyConI{} -> return ([], [])
        i -> error $ "unexpected TyCon: " ++ show i
  where
    unPlainTv (PlainTV tv) = Right tv
#if MIN_VERSION_template_haskell(2,8,0)
    unPlainTv (KindedTV i StarT) = Right i
#else
    unPlainTv (KindedTV i StarK) = Right i
#endif
    unPlainTv k@(KindedTV _ _) = Left k --  $ "unexpected non-plain TV" ++ show i

expandSyn ::  Type -> Q Type
expandSyn (ForallT tvs ctx t) = liftM (ForallT tvs ctx) $ expandSyn t
expandSyn t@AppT{} = expandSynApp t []
expandSyn t@ConT{} = expandSynApp t []
expandSyn (SigT t _) = expandSyn t   -- Ignore kind synonyms
expandSyn t = return t

expandSynApp :: Type -> [Type] -> Q Type
expandSynApp (AppT t1 t2) ts = do t2' <- expandSyn t2; expandSynApp t1 (t2':ts)
expandSynApp (ConT n) ts | nameBase n == "[]" = return $ foldl AppT ListT ts
expandSynApp t@(ConT n) ts = do
    info <- reify n
    case info of
        TyConI (TySynD _ tvs rhs) ->
            let (ts', ts'') = splitAt (length tvs) ts
                s = mkSubst tvs ts'
                rhs' = subst s rhs
            in  expandSynApp rhs' ts''
        _ -> return $ foldl AppT t ts
expandSynApp t ts = do t' <- expandSyn t; return $ foldl AppT t' ts


type Subst = [(Name, Type)]

mkSubst :: [TyVarBndr] -> [Type] -> Subst
mkSubst vs ts =
   let vs' = map un vs
       un (PlainTV v) = v
       un (KindedTV v _) = v
   in  assert (length vs' == length ts) $ zip vs' ts

subst :: Subst -> Type -> Type
subst s (ForallT v c t) = ForallT v c $ subst s t
subst s t@(VarT n) = fromMaybe t $ lookup n s
subst s (AppT t1 t2) = AppT (subst s t1) (subst s t2)
subst s (SigT t k) = SigT (subst s t) k
subst _ t = t

-- Written by Richard Eisenberg in th-desugar

tupleDegreeMaybe :: String -> Maybe Int
tupleDegreeMaybe s = do
    '(' : s1 <- return s
    (commas, ")") <- return $ span (== ',') s1
    let degree
          | "" <- commas = 0
          | otherwise    = length commas + 1
    return degree
