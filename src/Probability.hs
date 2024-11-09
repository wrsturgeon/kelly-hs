module Probability (Probability, unwrap) where

import Control.Functor.Linear (Applicative (pure, (<*>)), Functor (fmap), Monad ((>>=)), (<$>))
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.List.Linear ((++))
import Data.Num.Linear (AddIdentity (zero), Additive ((+)), AdditiveGroup ((-)), FromInteger (fromInteger), MultIdentity (one), Multiplicative ((*)), Num (abs, signum), Ring, Semiring)
import Data.Ord.Linear (Eq ((==)), Ord (compare))
import GHC.Num qualified as GHC
import Prelude.Linear (Floating (acos, acosh, asin, asinh, atan, atanh, cos, cosh, exp, log, pi, sin, sinh), Fractional (fromRational, (/)), Show (show))
import Scalar (Scalar)

-- why not have `Fractional` in the type signature?
-- read here: <https://stackoverflow.com/questions/72304990>
type Probability :: Type -> Type
newtype Probability :: Type -> Type where
  Probability :: Scalar a %1 -> Probability a

unwrap :: Probability a %1 -> Scalar a
unwrap (Probability s) = s

compose :: (b %1 -> c) %1 -> (a %1 -> b) %1 -> a %1 -> c
compose f g a = f (g a)

instance Data.Functor Probability where
  fmap f (Probability s) = Probability (f <$> s)

instance Functor Probability where
  fmap f (Probability s) = Probability (f <$> s)

instance Data.Applicative Probability where
  pure a = Probability (pure a)
  Probability f <*> Probability a = Probability (f <*> a)

instance Applicative Probability where
  pure a = Probability (pure a)
  Probability f <*> Probability a = Probability (f <*> a)

instance Monad Probability where
  Probability s >>= f = Probability (s >>= compose unwrap f)

instance (FromInteger a) => FromInteger (Probability a) where
  fromInteger i = Probability (fromInteger i)

instance (AddIdentity a) => AddIdentity (Probability a) where
  zero = Probability zero

instance (Additive a) => Additive (Probability a) where
  Probability a + Probability b = Probability (a + b)

instance (AdditiveGroup a) => AdditiveGroup (Probability a) where
  Probability a - Probability b = Probability (a - b)

instance (MultIdentity a) => MultIdentity (Probability a) where
  one = Probability one

instance (Multiplicative a) => Multiplicative (Probability a) where
  Probability a * Probability b = Probability (a * b)

instance (AdditiveGroup a, MultIdentity a) => Ring (Probability a)

instance (AddIdentity a, MultIdentity a) => Semiring (Probability a)

instance (Num a) => Num (Probability a) where
  abs = fmap abs
  signum = fmap signum

instance (Num a) => GHC.Num (Probability a) where
  Probability a + Probability b = Probability (a + b)
  Probability a - Probability b = Probability (a - b)
  Probability a * Probability b = Probability (a * b)
  abs (Probability a) = Probability (abs a)
  signum (Probability a) = Probability (signum a)
  fromInteger i = Probability (fromInteger i)

instance (Fractional a, Num a) => Fractional (Probability a) where
  fromRational r = Probability (fromRational r)
  Probability n / Probability d = Probability (n / d)

instance (Floating a, Num a) => Floating (Probability a) where
  pi = Probability pi
  exp (Probability a) = Probability (exp a)
  log (Probability a) = Probability (log a)
  sin (Probability a) = Probability (sin a)
  cos (Probability a) = Probability (cos a)
  asin (Probability a) = Probability (asin a)
  acos (Probability a) = Probability (acos a)
  atan (Probability a) = Probability (atan a)
  sinh (Probability a) = Probability (sinh a)
  cosh (Probability a) = Probability (cosh a)
  asinh (Probability a) = Probability (asinh a)
  acosh (Probability a) = Probability (acosh a)
  atanh (Probability a) = Probability (atanh a)

instance (Eq a) => Eq (Probability a) where
  Probability a == Probability b = a == b

instance (Ord a) => Ord (Probability a) where
  compare (Probability a) (Probability b) = compare a b

instance (FromInteger a, Multiplicative a, Show a) => Show (Probability a) where
  show (Probability a) = show a ++ " chance"
