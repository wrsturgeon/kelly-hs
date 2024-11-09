module Main (main) where

import Data.Kind (Type)
import Kelly (Contract (Contract, ifNo, ifYes), Marginal, Probability, Scalar, absolute, ifNo, ifYes)
import Kelly qualified (criterionDebug)
import Prelude.Linear (Double, IO, print, putStrLn)

type Number :: Type
type Number = Double

-- Using [this example](https://en.wikipedia.org/wiki/Kelly_criterion#Gambling_Formula):

gain :: Marginal Number
gain = 1.0

scaleIfYes :: Scalar Number
scaleIfYes = absolute gain

scaleIfNo :: Scalar Number
scaleIfNo = 0.0 -- 0.5

contract :: Contract (Scalar Number)
contract = Contract {ifYes = scaleIfYes, ifNo = scaleIfNo}

chance :: Probability Number
chance = 0.6

-- recommendedBet :: Scalar Number
-- recommendedBet = Kelly.criterion contract chance

main :: IO ()
main = do
  print contract
  print chance
  -- print recommendedBet
  putStrLn (Kelly.criterionDebug contract chance)
