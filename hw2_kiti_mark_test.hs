{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (SomeException, displayException, evaluate, try)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

-- Reuse the functions under test without touching their source file.
#include "hw2_kiti_mark.hs"

mkTest :: (Eq a, Show a) => String -> a -> a -> Test
mkTest label expected actual =
  TestLabel label $
    TestCase (actual @?= expected)

expectError :: (Show a) => String -> a -> a -> Test
expectError label reference actual =
  TestLabel label $
    TestCase $ do
      referenceOutcome <- tryEval reference
      actualOutcome <- tryEval actual
      case (referenceOutcome, actualOutcome) of
        (Left _, Left _) ->
          pure ()
        (Left refErr, Right actualValue) ->
          assertFailure $
            "reference threw "
              ++ displayException refErr
              ++ ", but implementation returned "
              ++ show actualValue
        (Right referenceValue, Left implErr) ->
          assertFailure $
            "reference returned "
              ++ show referenceValue
              ++ ", but implementation threw "
              ++ displayException implErr
        (Right referenceValue, Right actualValue) ->
          assertFailure $
            "reference returned "
              ++ show referenceValue
              ++ ", but implementation returned "
              ++ show actualValue
  where
    tryEval :: a -> IO (Either SomeException a)
    tryEval = try . evaluate

squareTests :: Test
squareTests =
  TestLabel "square" $
    TestList
      [ mkTest "squares zero" (0 :: Int) (square 0)
      , mkTest "squares positive numbers" (49 :: Int) (square 7)
      , mkTest "squares negative numbers" (36 :: Int) (square (-6))
      , let x = 123 :: Int
         in mkTest "matches repeated multiplication" (x * x) (square x)
      ]

halfTests :: Test
halfTests =
  TestLabel "half" $
    TestList
      [ mkTest "halves even numbers" (5 :: Double) (half (10 :: Double))
      , mkTest "halves odd counts" (2.5 :: Double) (half (5 :: Double))
      , mkTest "handles negative values" (-1.75 :: Double) (half (-3.5 :: Double))
      , let value = 42.0 :: Double
         in mkTest "double then half returns original" value (half (double value))
      ]

doubleTests :: Test
doubleTests =
  TestLabel "double" $
    TestList
      [ mkTest "doubles zero" (0 :: Int) (double 0)
      , mkTest "doubles positive numbers" (18 :: Int) (double (9 :: Int))
      , mkTest "doubles negative numbers" (-12 :: Int) (double (-6 :: Int))
      , let value = -13.5 :: Double
         in mkTest "half then double returns original" value (double (half value))
      ]

squareAreaTests :: Test
squareAreaTests =
  TestLabel "Square area" $
    TestList
      [ mkTest "zero length side has zero area" (0 :: Double) (area (Square 0))
      , let s = 5.5
         in mkTest "matches side squared" (s * s) (area (Square s))
      , let squares = [Square 1, Square 2.5, Square 3.25]
            expected = sum (map (\(Square sideLength) -> sideLength * sideLength) squares)
         in mkTest "sums areas polymorphically" expected (sum (map area squares))
      ]

circleAreaTests :: Test
circleAreaTests =
  TestLabel "Circle area" $
    TestList
      [ let r = 3.0
         in mkTest "matches pi r^2" (pi * r * r) (area (Circle r))
      , let small = 0.25
         in mkTest "handles fractional radii" (pi * small * small) (area (Circle small))
      , let circles = [Circle 0.5, Circle 2.0]
            expected = sum (map (\(Circle radiusLength) -> pi * radiusLength * radiusLength) circles)
         in mkTest "sums circle areas" expected (sum (map area circles))
      ]

petalAreaTests :: Test
petalAreaTests =
  TestLabel "Petal area" $
    TestList
      [ let s = 6.0
            halfSide = s / 2
            expected = pi * halfSide * halfSide - halfSide * halfSide
         in mkTest "subtracts inscribed square from circle" expected (area (Petal (Square s)))
      , let s = 2.5
            halfSide = s / 2
            expected = pi * halfSide * halfSide - halfSide * halfSide
         in mkTest "works for fractional petals" expected (area (Petal (Square s)))
      , mkTest "always yields positive area" True (area (Petal (Square 5)) > 0)
      ]

roseAreaTests :: Test
roseAreaTests =
  TestLabel "Rose area" $
    TestList
      [ let s = 8.0
            halfSide = s / 2
            petalArea = pi * halfSide * halfSide - halfSide * halfSide
            expected = 2 * petalArea
         in mkTest "doubles the underlying petal area" expected (area (Rose (Petal (Square s))))
      , let p = Petal (Square 3.5)
         in mkTest "rose always larger than single petal" True (area (Rose p) > area p)
      , let p = Petal (Square 12.0)
            rose = Rose p
            expected = double (area p)
         in mkTest "uses double helper for area" expected (area rose)
      ]

allTests :: Test
allTests =
  TestList
    [ squareTests
    , halfTests
    , doubleTests
    , squareAreaTests
    , circleAreaTests
    , petalAreaTests
    , roseAreaTests
    ]

main :: IO ()
main = do
  counts <- runTestTT allTests
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
