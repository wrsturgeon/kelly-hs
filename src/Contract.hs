module Contract (Contract (..), absolute, marginal) where

import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import Data.List.Linear ((++))
import Data.Num.Linear (Additive, AdditiveGroup, MultIdentity)
import Marginal (Marginal)
import Marginal qualified
import Prelude.Linear (Show (show))
import Scalar (Scalar)

type Contract :: Type -> Type
data Contract a = Contract {ifYes :: a, ifNo :: a}

instance Data.Functor Contract where
  fmap f (Contract {ifYes, ifNo}) = Contract {ifYes = f ifYes, ifNo = f ifNo}

-- it's NOT a `Control.Functor` because we use `f` twice:
-- instance Functor Contract where
--   fmap f (Contract {ifYes, ifNo}) = Contract {ifYes = f ifYes, ifNo = f ifNo}

instance (Show a) => Show (Contract a) where
  show (Contract y n) = "Contract { Y -> " ++ show y ++ " | N -> " ++ show n ++ " }"

marginal :: (MultIdentity a, AdditiveGroup a) => Contract (Scalar a) %1 -> Contract (Marginal a)
marginal = Data.fmap Marginal.marginal

absolute :: (Additive a, MultIdentity a) => Contract (Marginal a) %1 -> Contract (Scalar a)
absolute = Data.fmap Marginal.absolute
