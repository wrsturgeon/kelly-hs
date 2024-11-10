module Marginal (Marginal, absolute, marginal, unwrap) where

import Control.Functor.Linear (Applicative (pure, (<*>)), Functor (fmap), Monad ((>>=)), (<$>))
import Data.Eq qualified as ClassicalEq
import Data.Functor qualified as ClassicalFunctor
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.List.Linear ((++))
import Data.Num.Linear (AddIdentity (zero), Additive ((+)), AdditiveGroup ((-)), FromInteger (fromInteger), MultIdentity (one), Multiplicative ((*)), Num (abs, signum), Ring, Semiring)
import Data.Ord.Linear (Eq ((==)), Ord (compare, (<)))
import GHC.Num qualified as ClassicalNum
import Prelude.Linear (Floating (acos, acosh, asin, asinh, atan, atanh, cos, cosh, exp, log, pi, sin, sinh), Fractional (fromRational, (/)), Show (show))
import Scalar (Scalar)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))

type Marginal :: Type -> Type
newtype Marginal :: Type -> Type where
  Marginal :: Scalar a %1 -> Marginal a

marginal :: (MultIdentity a, AdditiveGroup a) => Scalar a %1 -> Marginal a
marginal s = Marginal (s - one)

absolute :: (MultIdentity a, Additive a) => Marginal a %1 -> Scalar a
absolute (Marginal s) = s + one

unwrap :: Marginal a %1 -> Scalar a
unwrap (Marginal s) = s

compose :: (b %1 -> c) %1 -> (a %1 -> b) %1 -> a %1 -> c
compose f g a = f (g a)

instance Data.Functor Marginal where
  fmap f (Marginal s) = Marginal (f <$> s)

instance Functor Marginal where
  fmap f (Marginal s) = Marginal (f <$> s)

instance ClassicalFunctor.Functor Marginal where
  fmap f (Marginal s) = Marginal (f ClassicalFunctor.<$> s)

instance Data.Applicative Marginal where
  pure a = Marginal (pure a)
  Marginal f <*> Marginal a = Marginal (f <*> a)

instance Applicative Marginal where
  pure a = Marginal (pure a)
  Marginal f <*> Marginal a = Marginal (f <*> a)

instance Monad Marginal where
  Marginal s >>= f = Marginal (s >>= compose unwrap f)

instance (FromInteger a) => FromInteger (Marginal a) where
  fromInteger i = Marginal (fromInteger i)

instance (AddIdentity a) => AddIdentity (Marginal a) where
  zero = Marginal zero

instance (Additive a) => Additive (Marginal a) where
  Marginal a + Marginal b = Marginal (a + b)

instance (AdditiveGroup a) => AdditiveGroup (Marginal a) where
  Marginal a - Marginal b = Marginal (a - b)

instance (MultIdentity a) => MultIdentity (Marginal a) where
  one = Marginal one

instance (Multiplicative a) => Multiplicative (Marginal a) where
  Marginal a * Marginal b = Marginal (a * b)

instance (AdditiveGroup a, MultIdentity a) => Ring (Marginal a)

instance (AddIdentity a, MultIdentity a) => Semiring (Marginal a)

instance (Num a) => Num (Marginal a) where
  abs = fmap abs
  signum = fmap signum

instance (Num a) => ClassicalNum.Num (Marginal a) where
  Marginal a + Marginal b = Marginal (a + b)
  Marginal a - Marginal b = Marginal (a - b)
  Marginal a * Marginal b = Marginal (a * b)
  abs (Marginal a) = Marginal (abs a)
  signum (Marginal a) = Marginal (signum a)
  fromInteger i = Marginal (fromInteger i)

instance (Fractional a, Num a) => Fractional (Marginal a) where
  fromRational r = Marginal (fromRational r)
  Marginal n / Marginal d = Marginal (n / d)

instance (Floating a, Num a) => Floating (Marginal a) where
  pi = Marginal pi
  exp (Marginal a) = Marginal (exp a)
  log (Marginal a) = Marginal (log a)
  sin (Marginal a) = Marginal (sin a)
  cos (Marginal a) = Marginal (cos a)
  asin (Marginal a) = Marginal (asin a)
  acos (Marginal a) = Marginal (acos a)
  atan (Marginal a) = Marginal (atan a)
  sinh (Marginal a) = Marginal (sinh a)
  cosh (Marginal a) = Marginal (cosh a)
  asinh (Marginal a) = Marginal (asinh a)
  acosh (Marginal a) = Marginal (acosh a)
  atanh (Marginal a) = Marginal (atanh a)

instance (Eq a) => Eq (Marginal a) where
  Marginal a == Marginal b = a == b

instance (Eq a) => ClassicalEq.Eq (Marginal a) where
  Marginal a == Marginal b = a == b

instance (Ord a) => Ord (Marginal a) where
  compare (Marginal a) (Marginal b) = compare a b

instance (Num a, Show a, Ord a) => Show (Marginal a) where
  show (Marginal s) = (if s < zero then "-" else "+") ++ show (abs s)

instance (Arbitrary a) => Arbitrary (Marginal a) where
  arbitrary = Marginal ClassicalFunctor.<$> arbitrary
  shrink (Marginal a) = Marginal ClassicalFunctor.<$> shrink a
