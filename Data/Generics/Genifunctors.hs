{-# LANGUAGE TemplateHaskell,PatternGuards #-}
module Data.Generics.Genifunctors where

import Language.Haskell.TH
import Control.Monad
import Data.List

simpCon :: Con -> (Name,[Type])
simpCon con = case con of
    NormalC n ts   -> (n,map snd ts)
    RecC n vts     -> (n,map (\ (_,_,t) -> t) vts)
    InfixC t1 n t2 -> (n,[snd t1,snd t2])
    ForallC{}      -> error "simpCon: ForallC"

-- | f = $(genFunctor T f)
genFunctor :: Name -> Name -> Q Exp
genFunctor tc_name f_name = do
    (tvs,cons) <- getTyConInfo tc_name
    fs <- zipWithM (const . newName) (repeat "_f") tvs
    x <- newName "_x"

    let ty  = foldl AppT (ConT tc_name) (map VarT tvs)
        rec = foldl AppE (VarE f_name) (map VarE fs)

    ms <- forM (map simpCon cons) $ \ (con_name,ts) -> do
        ys <- zipWithM (const . newName) (repeat "_y") ts
        let body = foldl AppE (ConE con_name)
                [ case t of
                    VarT a | Just i <- elemIndex a tvs -> AppE (VarE (fs !! i)) (VarE y)
                    _ | t == ty                        -> AppE rec (VarE y)
                    _                                  -> VarE y
                | (y,t) <- zip ys ts
                ]
        return $ Match (ConP con_name (map VarP ys)) (NormalB body) []

    return $ LamE (map VarP (fs ++ [x])) $ CaseE (VarE x) ms

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

getTyConInfo :: Name -> Q ([Name], [Con])
getTyConInfo con = do
    info <- reify con
    case info of
        TyConI (DataD _ _ tvs cs _) -> return (map unPlainTv tvs, cs)
        TyConI (NewtypeD _ _ tvs c _) -> return (map unPlainTv tvs, [c])
        PrimTyConI{} -> return ([], [])
        i -> error $ "unexpected TyCon: " ++ show i
  where
    unPlainTv (PlainTV tv) = tv
    unPlainTv i            = error $ "unexpected non-plain TV" ++ show i


