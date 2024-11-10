module Main (main) where

import Kelly (Contract (Contract, ifNo, ifYes), Marginal, Probability, Scalar, absolute, marginal)
import Kelly qualified (criterion)
import Prelude.Linear (Bool, Double, Fractional, IO, Int, Num, Ord, Show, String, abs, show, (++), (-), (.), (<), (==))
import Test.QuickCheck (Arbitrary, Property, property, withMaxSuccess)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck (testProperties)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

(=?) :: (Fractional a, Num a, Ord a) => a -> a -> Bool
a =? b = abs (a - b) < 1e-14

(=?!) :: (Fractional a, Num a, Ord a, Show a) => a -> a -> Assertion
a =?! b = assertBool (show a ++ " =? " ++ show b) (a =? b)

prop :: (Arbitrary a, Show a) => String -> (a -> Bool) -> (String, Property)
prop description f = (description, property (withMaxSuccess 100000 f))

properties :: TestTree
properties =
  testProperties
    "(checked by QuickCheck)"
    [ prop "marginal . absolute = id (Double)" (\(x :: Marginal Double) -> (marginal . absolute) x =? x),
      prop "marginal . absolute = id (Int)" (\(x :: Marginal Int) -> (marginal . absolute) x == x),
      prop "absolute . marginal = id (Double)" (\(x :: Scalar Double) -> (absolute . marginal) x =? x),
      prop "absolute . marginal = id (Int)" (\(x :: Scalar Int) -> (absolute . marginal) x == x)
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase
        "Wikipedia example (Double)"
        ( let gain = 1.0 :: Marginal Double
              chance = 0.6 :: Probability Double
              scaleIfYes = absolute gain
              contract = Contract {ifYes = scaleIfYes, ifNo = 0.0}
           in Kelly.criterion contract chance =?! 0.2
        )
    ]
