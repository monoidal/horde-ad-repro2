{-# LANGUAGE GHC2021, TypeOperators, DataKinds, GADTs #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -O -fworker-wrapper-cbv -dcore-lint #-}
module TensorClass where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, OrderingI (..), cmpNat, type (+), type (-), Nat)

data T (x :: Nat) = MkT

suncons :: forall n. (KnownNat n)
        => (() -> () -> () -> () -> T (1 + (n - 1)) -> T n)
        -> T n -> (T n, T n)
suncons sslice v = case cmpNat (Proxy @1) (Proxy @n) of
  LTI -> (sslice () () () () v,
          sslice () () () () v)
  _ -> undefined

