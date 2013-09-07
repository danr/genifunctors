{-# LANGUAGE TemplateHaskell,PatternGuards #-}
module Data.Generics.Genifunctors where

import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as M

simpCon :: Con -> (Name,[Type])
simpCon con = case con of
    NormalC n ts   -> (n,map snd ts)
    RecC n vts     -> (n,map (\ (_,_,t) -> t) vts)
    InfixC t1 n t2 -> (n,[snd t1,snd t2])
    ForallC{}      -> error "simpCon: ForallC"

-- | f = $(genFunctor T)
genFunctor :: Name -> Q Exp
genFunctor tc = do
    (fn,decls) <- runWriterT $ genMultiFunctor tc `evalStateT` M.empty
    return $ LetE decls (VarE fn)

type GenM = StateT (Map Name Name) (WriterT [Dec] Q)

genMatch :: Type -> [(Name,Name)] -> GenM Exp
genMatch t tvfs = case t of
    VarT a     | Just f <- lookup a tvfs -> return (VarE f)
    AppT t1 t2 -> AppE <$> genMatch t1 tvfs <*> genMatch t2 tvfs
    ConT tc    -> VarE <$> genMultiFunctor tc
    TupleT i   -> VarE <$> genMultiFunctor (tupleTypeName i)
    ListT      -> VarE <$> genMultiFunctor ''[]
    _          -> error $ "genMatch:" ++ show t

genMultiFunctor :: Name -> GenM Name
genMultiFunctor tc = do
    m_fn <- gets (M.lookup tc)
    case m_fn of
        Just fn -> return fn
        Nothing -> do
            fn <- q $ newName ("_" ++ nameBase tc)
            modify (M.insert tc fn)
            (tvs,cons) <- getTyConInfo tc
            fs <- zipWithM (const . q . newName) (repeat "_f") tvs
            x <- q $ newName "_x"

            ms <-
                if null cons || null tvs
                    then return [Match WildP (NormalB $ VarE x) []]  -- primitive data types/non polymorphic
                    else forM (map simpCon cons) $ \ (con_name,ts) -> do
                        ys <- zipWithM (const . q . newName) (repeat "_y") ts
                        body <- foldl AppE (ConE con_name) <$> sequence
                                [ do le <- genMatch t (zip tvs fs)
                                     return (le `AppE` VarE y)
                                | (y,t) <- zip ys ts ]

                        return $ Match (ConP con_name (map VarP ys)) (NormalB body) []

            from <- mapM (q . newName . nameBase) tvs
            to   <- mapM (q . newName . nameBase) tvs
            let ty = ForallT (map PlainTV (from ++ to)) []
                   $ foldr arr
                        (applyTyVars tc from `arr` applyTyVars tc to)
                        (zipWith arr (map VarT from) (map VarT to))

            tell
                [ SigD fn ty
                , FunD fn
                    [ Clause
                        (map VarP (fs ++ [x]))
                        (NormalB $ CaseE (VarE x) $ ms)
                        []
                    ]
                ]
            return fn

arr :: Type -> Type -> Type
arr t1 t2 = (ArrowT `AppT` t1) `AppT` t2

applyTyVars :: Name -> [Name] -> Type
applyTyVars tc ns = foldl AppT (ConT tc) (map VarT ns)

q :: Q a -> GenM a
q = lift . lift

getTyConInfo :: Name -> GenM ([Name], [Con])
getTyConInfo con = do
    info <- q (reify con)
    case info of
        TyConI (DataD _ _ tvs cs _) -> return (map unPlainTv tvs, cs)
        TyConI (NewtypeD _ _ tvs c _) -> return (map unPlainTv tvs, [c])
        PrimTyConI{} -> return ([], [])
        i -> error $ "unexpected TyCon: " ++ show i
  where
    unPlainTv (PlainTV tv) = tv
    unPlainTv i            = error $ "unexpected non-plain TV" ++ show i







{-
-- | name has type
-- (x1 -> x1') -> ... -> (xn -> xn') -> T x1 .. xn -> T x1 ... xn'
getFunctorTypeInfo :: Name -> Q ([(Name,Name)],Name)
getFunctorTypeInfo name = do
    info <- reify name
    let strip []  (ForallT _tvs [] t)           = strip [] t
        strip acc ((ArrowT `AppT` i) `AppT` o) = case i of
            (ArrowT `AppT` VarT x) `AppT` VarT x' -> strip ((x,x'):acc) o
            _                                     -> (reverse acc,tycon acc i o)
        strip _ _ = error "strip"

        tycon ((x,x'):acc) (AppT t (VarT y)) (AppT t' (VarT y')) | x == y && x' == y' = tycon acc t t'
        tycon []           (ConT tc)         (ConT tc')          | tc == tc'          = tc
        tycon _ _ _ = error "tycon"
    case info of
        VarI _ t _ _ -> return $ strip [] t
        _            -> error "info"
        -}
