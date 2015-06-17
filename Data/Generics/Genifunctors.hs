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
import Data.Char

type GenM = RWST Generator [Dec] (Map Name Name) Q

data Generator = Generator
    { gen_combine   :: Name -> [Exp] -> Exp
    , gen_primitive :: Exp -> Exp
    , gen_type      :: Name -> [Name] -> Q Type
    }

gen :: Generator -> Name -> Q Exp
gen g = genT g []

genT :: Generator -> [(Name,Name)] -> Name -> Q Exp
genT generator predef tc = do
    forM_ predef $ \(con,_) -> do
      info <- reify con
      case info of
        TyConI TySynD{} -> fail $ show con ++ " is a type synonym."
        TyConI _        -> return ()
        _               -> fail $ show con ++ " is not a type constructor."
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
genFoldMapT = genT Generator
    { gen_combine   = foldMapCombine
    , gen_primitive = foldMapPrimitive
    , gen_type      = foldMapType
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
--travTupleRev f g (a,b) = (\\b a -> (a,b)) \<$\> g b \<*\> f a
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

fmapType :: Name -> [Name] -> Q Type
fmapType tc tvs = do
    from <- mapM (newName . nameBase) tvs
    to   <- mapM (newName . nameBase) tvs
    return $ ForallT (map PlainTV (from ++ to)) []
           $ foldr arr
                (applyTyVars tc from `arr` applyTyVars tc to)
                (zipWith arr (map VarT from) (map VarT to))

foldMapType :: Name -> [Name] -> Q Type
foldMapType tc tvs = do
    m <- newName "m"
    from <- mapM (newName . nameBase) tvs
    return $ ForallT (map PlainTV (m : from)) [classType ''Monoid (VarT m)]
           $ foldr arr
                (applyTyVars tc from `arr` VarT m)
                (zipWith arr (map VarT from) (repeat (VarT m)))

traverseType :: Name -> Name -> [Name] -> Q Type
traverseType constraint_class tc tvs = do
    f <- newName "f"
    from <- mapM (newName . nameBase) tvs
    to   <- mapM (newName . nameBase) tvs
    return $ ForallT (map PlainTV (f : from ++ to)) [classType constraint_class (VarT f)]
           $ foldr arr
                (applyTyVars tc from `arr` (VarT f `AppT` applyTyVars tc to))
                (zipWith arr (map VarT from) (map (\ t -> VarT f `AppT` VarT t) to))

genMatch :: Type -> [(Name,Name)] -> GenM Exp
genMatch t tvfs = case t of
    VarT a     | Just f <- lookup a tvfs -> return (VarE f)
    AppT t1 t2 -> AppE <$> genMatch t1 tvfs <*> genMatch t2 tvfs
    ConT tc    -> VarE <$> generate tc
    TupleT i   -> VarE <$> generate (tupleTypeName i)
    ListT      -> VarE <$> generate ''[]
    _          -> error $ "genMatch:" ++ show t

simpCon :: Con -> (Name,[Type])
simpCon con = case con of
    NormalC n ts   -> (n,map snd ts)
    RecC n vts     -> (n,map (\ (_,_,t) -> t) vts)
    InfixC t1 n t2 -> (n,[snd t1,snd t2])
    ForallC{}      -> error "simpCon: ForallC"


generate :: Name -> GenM Name
generate tc = do
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
                                     le <- genMatch t' (zip tvs fs)
                                     return (le `AppE` VarE y)
                                | (y,t) <- zip ys ts ]

                        return $ Match (ConP con_name (map VarP ys)) (NormalB lhs) []

                    return (CaseE (VarE x) matches)

            ty <- q $ gen_type tc tvs

            tell
                [ SigD fn ty
                , FunD fn [ Clause (map VarP (fs ++ [x])) (NormalB $ body) [] ]
                ]
            return fn


arr :: Type -> Type -> Type
arr t1 t2 = (ArrowT `AppT` t1) `AppT` t2

applyTyVars :: Name -> [Name] -> Type
applyTyVars tc ns = foldl AppT (ConT tc) (map VarT ns)

q :: Q a -> GenM a
q = lift

#if MIN_VERSION_template_haskell(2,10,0)
classType :: Name -> Type -> Type
classType name inst = AppT (ConT name) inst
#else
classType :: Name -> Type -> Pred
classType name inst = ClassP name [inst]
#endif

-- Contributed by Víctor López Juan, https://lopezjuan.com/
newSanitizedName :: String -> Q Name
newSanitizedName nb = newName $ '_':'N':(
    nb >>= \x -> case x of
      c | isAlphaNum c || c == '\''-> [c]
      '_' -> "__"
      c   -> "_" ++ show (ord c))

-- All the following functions are (originally) by Lennart in Geniplate
-- Move these to a TH-util package?

getTyConInfo :: Name -> GenM ([Name], [Con])
getTyConInfo con = do
    info <- q (reify con)
    case info of
        TyConI (DataD _ _ tvs cs _) -> return (map unPlainTv tvs, cs)
        TyConI (NewtypeD _ _ tvs c _) -> return (map unPlainTv tvs, [c])
        PrimTyConI{} -> return ([], [])
        i -> error $ "unexpected TyCon: " ++ show i
  where
    unPlainTv (PlainTV tv)        = tv
#if MIN_VERSION_template_haskell(2,8,0)
    unPlainTv (KindedTV tv StarT) = tv
#else
    unPlainTv (KindedTV tv StarK) = tv
#endif
    unPlainTv i                   = error $ "unexpected non-plain TV" ++ show i

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

