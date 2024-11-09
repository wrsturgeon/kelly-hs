module Kelly (Contract (..), Marginal, Probability, Scalar, absolute, absoluteContract, criterion, criterionDebug, criterionMarginal, criterionMarginalDebug, marginal, marginalContract, scale) where

import Contract (Contract (Contract, ifNo, ifYes))
import Contract qualified (absolute, marginal)
import Criterion qualified (absolute, absoluteDebug, marginal, marginalDebug)
import Data.Num.Linear (Additive, AdditiveGroup, MultIdentity, Num)
import Marginal (Marginal, absolute, marginal)
import Prelude.Linear (Fractional, Show, String)
import Probability (Probability)
import Scalar (Scalar, scale)

absoluteContract :: (Additive a, MultIdentity a) => Contract (Marginal a) %1 -> Contract (Scalar a)
absoluteContract = Contract.absolute

marginalContract :: (AdditiveGroup a, MultIdentity a) => Contract (Scalar a) %1 -> Contract (Marginal a)
marginalContract = Contract.marginal

criterion :: (Fractional a, Num a) => Contract (Scalar a) -> Probability a -> Scalar a
criterion = Criterion.absolute

criterionDebug :: (Fractional a, Num a, Show a) => Contract (Scalar a) -> Probability a -> String
criterionDebug = Criterion.absoluteDebug

criterionMarginal :: (Fractional a, Num a) => Contract (Marginal a) -> Probability a -> Scalar a
criterionMarginal = Criterion.marginal

criterionMarginalDebug :: (Fractional a, Num a, Show a) => Contract (Marginal a) -> Probability a -> String
criterionMarginalDebug = Criterion.marginalDebug
