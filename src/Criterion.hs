module Criterion (absolute, absoluteDebug, marginal, marginalDebug) where

import Contract qualified
import Data.List.Linear ((++))
import Data.Num.Linear (AdditiveGroup ((-)), MultIdentity (one), Num)
import Marginal qualified
import Prelude.Linear (Fractional ((/)), Show (show), String)
import Probability qualified
import Scalar (Scalar)

marginal :: (Fractional a, Num a) => Contract.Contract (Marginal.Marginal a) -> Probability.Probability a -> Scalar a
marginal c probabilityOfYes =
  (pYes / loss) - (pNo / gain)
  where
    pYes = Probability.unwrap probabilityOfYes
    pNo = one - pYes
    gain = Marginal.unwrap (Contract.ifYes c)
    loss = one - Marginal.unwrap (Contract.ifNo c)

marginalDebug :: (Fractional a, Num a, Show a) => Contract.Contract (Marginal.Marginal a) -> Probability.Probability a -> String
marginalDebug c probabilityOfYes =
  "(" ++ show pYes ++ " / " ++ show loss ++ ") - (" ++ show pNo ++ " / " ++ show gain ++ ") = " ++ show ((pYes / loss) - (pNo / gain))
  where
    pYes = Probability.unwrap probabilityOfYes
    pNo = one - pYes
    gain = Marginal.unwrap (Contract.ifYes c)
    loss = -Marginal.unwrap (Contract.ifNo c)

absolute :: (Fractional a, Num a) => Contract.Contract (Scalar a) -> Probability.Probability a -> Scalar a
absolute c = marginal (Contract.marginal c)

absoluteDebug :: (Fractional a, Num a, Show a) => Contract.Contract (Scalar a) -> Probability.Probability a -> String
absoluteDebug c = marginalDebug (Contract.marginal c)
