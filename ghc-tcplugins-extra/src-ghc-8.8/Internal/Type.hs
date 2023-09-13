module Internal.Type (substType) where

import Data.Maybe (fromMaybe)
import GhcPlugins hiding (TcPlugin, mkSubst)
import TcType (TcType)
import TyCoRep (Type (..))

-- | Apply substitutions in Types
--
-- __NB:__ Doesn't substitute under binders
substType
  :: [(TcTyVar, TcType)]
  -> TcType
  -> TcType
substType subst tv@(TyVarTy v) =
  fromMaybe tv (lookup v subst)
substType subst (AppTy t1 t2) =
  AppTy (substType subst t1) (substType subst t2)
substType subst (TyConApp tc xs) =
  TyConApp tc (map (substType subst) xs)
substType _subst t@(ForAllTy _tv _ty) =
  -- TODO: Is it safe to do "dumb" substitution under binders?
  -- ForAllTy tv (substType subst ty)
  t
substType subst (FunTy t1 t2) =
  FunTy (substType subst t1) (substType subst t2)
substType _ l@(LitTy _) = l
substType subst (CastTy ty co) =
  CastTy (substType subst ty) co
substType _ co@(CoercionTy _) = co
