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

mkApproxTest :: String -> Double -> Double -> Double -> Test
mkApproxTest label tolerance expected actual =
  TestLabel label $
    TestCase $
      let difference = abs (expected - actual)
       in assertBool
            (label ++ " expected " ++ show expected ++ " but was " ++ show actual)
            (difference <= tolerance)

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

roseThreshold :: Double
roseThreshold = 0.1

sumHalvingSeries :: Double -> Double
sumHalvingSeries start = go start
  where
    go current
      | current < roseThreshold = current
      | otherwise = current + go (current / 2)

squareTests :: Test
squareTests =
  TestLabel "square" $
    TestList
      [ mkTest "squares zero" (0 :: Int) (square 0),
        mkTest "squares positive numbers" (49 :: Int) (square 7),
        mkTest "squares negative numbers" (36 :: Int) (square (-6)),
        let x = 123 :: Int
         in mkTest "matches repeated multiplication" (x * x) (square x)
      ]

halfTests :: Test
halfTests =
  TestLabel "half" $
    TestList
      [ mkTest "halves even numbers" (5 :: Double) (half (10 :: Double)),
        mkTest "halves odd counts" (2.5 :: Double) (half (5 :: Double)),
        mkTest "handles negative values" (-1.75 :: Double) (half (-3.5 :: Double)),
        let value = 42.0 :: Double
         in mkTest "double then half returns original" value (half (double value))
      ]

doubleTests :: Test
doubleTests =
  TestLabel "double" $
    TestList
      [ mkTest "doubles zero" (0 :: Int) (double 0),
        mkTest "doubles positive numbers" (18 :: Int) (double (9 :: Int)),
        mkTest "doubles negative numbers" (-12 :: Int) (double (-6 :: Int)),
        let value = -13.5 :: Double
         in mkTest "half then double returns original" value (double (half value))
      ]

hypotenuseTests :: Test
hypotenuseTests =
  TestLabel "hypotenuse" $
    TestList
      [ mkTest "computes 3-4-5 triangle" (5 :: Double) (hypotenuse (3 :: Double) (4 :: Double)),
        mkTest "returns zero when both legs are zero" (0 :: Double) (hypotenuse (0 :: Double) (0 :: Double)),
        let leg = 5.5 :: Double
            expected = leg * sqrt 2
         in mkApproxTest "matches diagonal of a square" 1e-12 expected (hypotenuse leg leg)
      ]

squareAreaTests :: Test
squareAreaTests =
  TestLabel "Square area" $
    TestList
      [ mkTest "zero length side has zero area" (0 :: Double) (area (Square 0)),
        let s = 5.5
         in mkTest "matches side squared" (s * s) (area (Square s)),
        let squares = [Square 1, Square 2.5, Square 3.25]
            expected = sum (map (\(Square sideLength) -> sideLength * sideLength) squares)
         in mkTest "sums areas polymorphically" expected (sum (map area squares))
      ]

circleAreaTests :: Test
circleAreaTests =
  TestLabel "Circle area" $
    TestList
      [ let r = 3.0
         in mkTest "matches pi r^2" (pi * r * r) (area (Circle r)),
        let small = 0.25
         in mkTest "handles fractional radii" (pi * small * small) (area (Circle small)),
        let circles = [Circle 0.5, Circle 2.0]
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
         in mkTest "subtracts inscribed square from circle" expected (area (Petal (Square s))),
        let s = 2.5
            halfSide = s / 2
            expected = pi * halfSide * halfSide - halfSide * halfSide
         in mkTest "works for fractional petals" expected (area (Petal (Square s))),
        mkTest "always yields positive area" True (area (Petal (Square 5)) > 0)
      ]

petalInnerTests :: Test
petalInnerTests =
  TestLabel "Petal inner" $
    TestList
      [ let bg = Square 10.0
            initialPetal = Petal bg
            innerPetal = inner initialPetal
            expectedSide = hypotenuse (half (side bg)) (half (side bg))
         in mkTest "uses hypotenuse of half-length sides" expectedSide (side (background innerPetal)),
        let base = Petal (Square 6.0)
         in mkTest "inner petal shrinks total area" True (area (inner base) < area base),
        let base = Petal (Square 6.0)
            expectedArea = area base / 2
            actualArea = area (inner base)
         in mkApproxTest "inner petal area is half of base petal" 1e-12 expectedArea actualArea
      ]

roseAreaTests :: Test
roseAreaTests =
  TestLabel "Rose area" $
    TestList
      [ let s = 8.0
            halfSide = s / 2
            petalArea = pi * halfSide * halfSide - halfSide * halfSide
            expected = 2 * petalArea
         in mkTest "doubles the underlying petal area" expected (area (Rose (Petal (Square s)))),
        let p = Petal (Square 3.5)
         in mkTest "rose always larger than single petal" True (area (Rose p) > area p),
        let p = Petal (Square 12.0)
            rose = Rose p
            expected = double (area p)
         in mkTest "uses double helper for area" expected (area rose)
      ]

roseInnerTests :: Test
roseInnerTests =
  TestLabel "Rose inner" $
    TestList
      [ let basePetal = Petal (Square 9.0)
            baseRose = Rose basePetal
         in mkTest "delegates inner call to petal" (inner basePetal) (petal (inner baseRose)),
        let basePetal = Petal (Square 4.0)
            baseRose = Rose basePetal
            innerPetal = inner basePetal
            expectedArea = double (area innerPetal)
            actualArea = area (inner baseRose)
         in mkApproxTest "inner rose area is half of base rose" 1e-12 expectedArea actualArea
      ]

roseRecursiveAreaTests :: Test
roseRecursiveAreaTests =
  TestLabel "Rose recursive area" $
    TestList
      [ let sideLength = 0.4
            rose = Rose (Petal (Square sideLength))
            expected = area (petal rose)
         in mkApproxTest "returns petal area when already below threshold" 1e-12 expected (roseAreaWithRecurison rose),
        let sideLength = 10.0
            rose = Rose (Petal (Square sideLength))
            firstPetalArea = area (petal rose)
            expected = sumHalvingSeries firstPetalArea
         in mkApproxTest "sums halved petal areas until threshold" 1e-12 expected (roseAreaWithRecurison rose)
      ]

singleAVLNodeTests :: Test
singleAVLNodeTests =
  TestLabel "singleAVLNode" $
    TestList
      [ let expected = Node 42 EmptyAVLTree EmptyAVLTree
         in mkTest "wraps value with empty children" expected (singleAVLNode 42),
        let actual = singleAVLNode (7 :: Int)
            expected = Node (7 :: Int) EmptyAVLTree EmptyAVLTree
         in mkTest "matches manual node construction" expected actual
      ]

insertAVLNodeTests :: Test
insertAVLNodeTests =
  TestLabel "insertAVLNode" $
    TestList
      [ let expected = singleAVLNode 10
         in mkTest "inserts into empty tree" expected (insertAVLNode EmptyAVLTree 10),
        let base = singleAVLNode (10 :: Int)
            expected = Node 10 (singleAVLNode 5) EmptyAVLTree
         in mkTest "places smaller values on the left" expected (insertAVLNode base 5),
        let base = insertAVLNode (singleAVLNode (10 :: Int)) 5
            expected = Node 10 (singleAVLNode 5) (singleAVLNode 15)
         in mkTest "places larger values on the right" expected (insertAVLNode base 15),
        let base = Node 3 (Node 2 EmptyAVLTree EmptyAVLTree) EmptyAVLTree
            expected = Node 2 (Node 1 EmptyAVLTree EmptyAVLTree) (Node 3 EmptyAVLTree EmptyAVLTree)
         in mkTest "left-left rebalance" expected (insertAVLNode base 1),
        let base = Node 3 (Node 1 EmptyAVLTree EmptyAVLTree) EmptyAVLTree
            expected = Node 2 (Node 1 EmptyAVLTree EmptyAVLTree) (Node 3 EmptyAVLTree EmptyAVLTree)
         in mkTest "left-right rebalance" expected (insertAVLNode base 2),
        let base = Node 1 EmptyAVLTree (Node 2 EmptyAVLTree EmptyAVLTree)
            expected = Node 2 (Node 1 EmptyAVLTree EmptyAVLTree) (Node 3 EmptyAVLTree EmptyAVLTree)
         in mkTest "right-right rebalance" expected (insertAVLNode base 3),
        let base = Node 1 EmptyAVLTree (Node 3 EmptyAVLTree EmptyAVLTree)
            expected = Node 2 (Node 1 EmptyAVLTree EmptyAVLTree) (Node 3 EmptyAVLTree EmptyAVLTree)
         in mkTest "right-right rebalance" expected (insertAVLNode base 2)
      ]

avlTreeWalkthroughTests :: Test
avlTreeWalkthroughTests =
  TestLabel "AVLTree" $
    TestList
      [ mkTest "step1 - insert 1" expected1 step1,
        mkTest "step1 - length" 1 (length step1),
        mkTest "step2 - insert 2" expected2 step2,
        mkTest "step2 - length" 2 (length step2),
        mkTest "step3 - insert 3" expected3 step3,
        mkTest "step3 - length" 3 (length step3),
        mkTest "step4 - insert 5" expected4 step4,
        mkTest "step4 - length" 4 (length step4),
        mkTest "step5 - insert -1" expected5 step5,
        mkTest "step5 - length" 5 (length step5),
        mkTest "step6 - insert 6" expected6 step6,
        mkTest "step6 - length" 6 (length step6),
        mkTest "step7 - insert 8" expected7 step7,
        mkTest "step7 - length" 7 (length step7),
        mkTest "step8 - insert 10" expected8 step8,
        mkTest "step8 - length" 8 (length step8),
        mkTest "step9 - insert 11" expected9 step9,
        mkTest "step9 - length" 9 (length step9),
        mkTest "step10 - insert 4" expected10 step10,
        mkTest "step10 - length" 10 (length step10),
        mkTest "step11 - insert 7" expected11 step11,
        mkTest "step11 - length" 11 (length step11)
      ]
  where
    step0 = EmptyAVLTree :: AVLTree Int
    step1 = insertAVLNode step0 1
    step2 = insertAVLNode step1 2
    step3 = insertAVLNode step2 3
    step4 = insertAVLNode step3 5
    step5 = insertAVLNode step4 (-1)
    step6 = insertAVLNode step5 6
    step7 = insertAVLNode step6 8
    step8 = insertAVLNode step7 10
    step9 = insertAVLNode step8 11
    step10 = insertAVLNode step9 4
    step11 = insertAVLNode step10 7

    expected1 = Node 1 EmptyAVLTree EmptyAVLTree
    expected2 = Node 1 EmptyAVLTree (singleAVLNode 2)
    expected3 = Node 2 (singleAVLNode 1) (singleAVLNode 3)
    expected4 = Node 2 (singleAVLNode 1) (Node 3 EmptyAVLTree (singleAVLNode 5))
    expected5 =
      Node
        2
        (Node 1 (singleAVLNode (-1)) EmptyAVLTree)
        (Node 3 EmptyAVLTree (singleAVLNode 5))
    expected6 =
      Node
        2
        (Node 1 (singleAVLNode (-1)) EmptyAVLTree)
        (Node 5 (singleAVLNode 3) (singleAVLNode 6))
    expected7 =
      Node
        2
        (Node 1 (singleAVLNode (-1)) EmptyAVLTree)
        (Node 5 (singleAVLNode 3) (Node 6 EmptyAVLTree (singleAVLNode 8)))
    expected8 =
      Node
        2
        (Node 1 (singleAVLNode (-1)) EmptyAVLTree)
        (Node 5 (singleAVLNode 3) (Node 8 (singleAVLNode 6) (singleAVLNode 10)))
    expected9 =
      Node
        2
        (Node 1 (singleAVLNode (-1)) EmptyAVLTree)
        ( Node
            8
            (Node 5 (singleAVLNode 3) (singleAVLNode 6))
            (Node 10 EmptyAVLTree (singleAVLNode 11))
        )
    expected10 =
      Node
        5
        ( Node
            2
            (Node 1 (singleAVLNode (-1)) EmptyAVLTree)
            ( Node
                3
                EmptyAVLTree
                (singleAVLNode 4)
            )
        )
        ( Node
            8
            (singleAVLNode 6)
            ( Node
                10
                EmptyAVLTree
                (singleAVLNode 11)
            )
        )
    expected11 =
      Node
        5
        ( Node
            2
            (Node 1 (singleAVLNode (-1)) EmptyAVLTree)
            ( Node
                3
                EmptyAVLTree
                (singleAVLNode 4)
            )
        )
        ( Node
            8
            ( Node
                6
                EmptyAVLTree
                (singleAVLNode 7)
            )
            ( Node
                10
                EmptyAVLTree
                (singleAVLNode 11)
            )
        )

balanceFactorTests :: Test
balanceFactorTests =
  TestLabel "balanceFactor" $
    TestList
      [ mkTest "leaf nodes are balanced" 0 (balanceFactor (singleAVLNode (1 :: Int))),
        let base = Node 8 EmptyAVLTree EmptyAVLTree
         in mkTest "symmetric three-node tree has 0 balance - 1st" 0 (balanceFactor base),
        let base =
              Node
                8
                (Node 4 EmptyAVLTree EmptyAVLTree)
                (Node 9 EmptyAVLTree EmptyAVLTree)
         in mkTest "symmetric three-node tree has 0 balance - 2rd" 0 (balanceFactor base),
        let base = Node 8 (Node 4 EmptyAVLTree EmptyAVLTree) EmptyAVLTree
         in mkTest "unbalance left-node tree has -1 balance" (-1) (balanceFactor base),
        let base =
              Node
                8
                EmptyAVLTree
                (Node 9 EmptyAVLTree EmptyAVLTree)
         in mkTest "unbalance right-node tree has 1 balance" 1 (balanceFactor base),
        let base =
              Node
                8
                ( Node
                    4
                    (Node 2 EmptyAVLTree EmptyAVLTree)
                    EmptyAVLTree
                )
                EmptyAVLTree
         in mkTest "unbalance left-node tree has -2 balance" (-2) (balanceFactor base),
        let base =
              Node
                8
                EmptyAVLTree
                ( Node
                    10
                    EmptyAVLTree
                    (Node 12 EmptyAVLTree EmptyAVLTree)
                )
         in mkTest "unbalance left-node tree has 2 balance" 2 (balanceFactor base)
      ]

heightTreeTests :: Test
heightTreeTests =
  TestLabel "heightTree" $
    TestList
      [ mkTest "empty tree has height zero" 0 (heightTree (EmptyAVLTree :: AVLTree Int)),
        mkTest "single node tree has height one" 1 (heightTree (singleAVLNode (42 :: Int))),
        let tree =
              Node
                (10 :: Int)
                ( Node
                    5
                    (singleAVLNode 2)
                    (singleAVLNode 7)
                )
                (singleAVLNode 15)
         in mkTest "height tracks deepest branch" 3 (heightTree tree)
      ]

allTests :: Test
allTests =
  TestList
    [ squareTests,
      halfTests,
      doubleTests,
      hypotenuseTests,
      squareAreaTests,
      circleAreaTests,
      petalAreaTests,
      petalInnerTests,
      roseAreaTests,
      roseInnerTests,
      roseRecursiveAreaTests,
      singleAVLNodeTests,
      insertAVLNodeTests,
      avlTreeWalkthroughTests,
      balanceFactorTests,
      heightTreeTests
    ]

main :: IO ()
main = do
  counts <- runTestTT allTests
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
