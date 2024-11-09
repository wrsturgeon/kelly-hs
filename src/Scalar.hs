module Scalar (Scalar, scale) where

import Control.Functor.Linear (Applicative (pure, (<*>)), Functor (fmap), Monad ((>>=)))
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.List.Linear ((++))
import Data.Num.Linear (AddIdentity (zero), Additive ((+)), AdditiveGroup ((-)), FromInteger (fromInteger), MultIdentity (one), Multiplicative ((*)), Num (abs, signum), Ring, Semiring)
import Data.Ord.Linear (Eq ((==)), Ord (compare))
import GHC.Num qualified as GHC
import Prelude.Linear (Floating (acos, acosh, asin, asinh, atan, atanh, cos, cosh, exp, log, pi, sin, sinh), Fractional (fromRational, (/)), Show (show))

-- why not have `Fractional` in the type signature?
-- read here: <https://stackoverflow.com/questions/72304990>
type Scalar :: Type -> Type
newtype Scalar :: Type -> Type where
  Scalar :: a %1 -> Scalar a

instance Data.Functor Scalar where
  fmap f (Scalar a) = Scalar (f a)

instance Functor Scalar where
  fmap f (Scalar a) = Scalar (f a)

instance Data.Applicative Scalar where
  pure = Scalar
  Scalar f <*> Scalar a = Scalar (f a)

instance Applicative Scalar where
  pure = Scalar
  Scalar f <*> Scalar a = Scalar (f a)

instance Monad Scalar where
  Scalar a >>= f = f a

instance (FromInteger a) => FromInteger (Scalar a) where
  fromInteger i = Scalar (fromInteger i)

instance (AddIdentity a) => AddIdentity (Scalar a) where
  zero = Scalar zero

instance (Additive a) => Additive (Scalar a) where
  Scalar a + Scalar b = Scalar (a + b)

instance (AdditiveGroup a) => AdditiveGroup (Scalar a) where
  Scalar a - Scalar b = Scalar (a - b)

instance (MultIdentity a) => MultIdentity (Scalar a) where
  one = Scalar one

instance (Multiplicative a) => Multiplicative (Scalar a) where
  Scalar a * Scalar b = Scalar (a * b)

instance (AdditiveGroup a, MultIdentity a) => Ring (Scalar a)

instance (AddIdentity a, MultIdentity a) => Semiring (Scalar a)

instance (Num a) => Num (Scalar a) where
  abs = fmap abs
  signum = fmap signum

instance (Num a) => GHC.Num (Scalar a) where
  Scalar a + Scalar b = Scalar (a + b)
  Scalar a - Scalar b = Scalar (a - b)
  Scalar a * Scalar b = Scalar (a * b)
  abs (Scalar a) = Scalar (abs a)
  signum (Scalar a) = Scalar (signum a)
  fromInteger i = Scalar (fromInteger i)

instance (Fractional a, Num a) => Fractional (Scalar a) where
  fromRational r = Scalar (fromRational r)
  Scalar n / Scalar d = Scalar (n / d)

instance (Floating a, Num a) => Floating (Scalar a) where
  pi = Scalar pi
  exp (Scalar a) = Scalar (exp a)
  log (Scalar a) = Scalar (log a)
  sin (Scalar a) = Scalar (sin a)
  cos (Scalar a) = Scalar (cos a)
  asin (Scalar a) = Scalar (asin a)
  acos (Scalar a) = Scalar (acos a)
  atan (Scalar a) = Scalar (atan a)
  sinh (Scalar a) = Scalar (sinh a)
  cosh (Scalar a) = Scalar (cosh a)
  asinh (Scalar a) = Scalar (asinh a)
  acosh (Scalar a) = Scalar (acosh a)
  atanh (Scalar a) = Scalar (atanh a)

instance (Eq a) => Eq (Scalar a) where
  Scalar a == Scalar b = a == b

instance (Ord a) => Ord (Scalar a) where
  compare (Scalar a) (Scalar b) = compare a b

instance (FromInteger a, Multiplicative a, Show a) => Show (Scalar a) where
  show (Scalar a) = show (a * fromInteger 100) ++ "%"

scale :: (Multiplicative a, Functor f) => Scalar a %1 -> f a %1 -> f a
scale (Scalar a) = fmap (a *)
