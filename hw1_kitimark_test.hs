{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Char (toUpper)
import Data.List
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

-- Reuse the functions under test without touching their source file.
#include "hw1_kitimark.hs"

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

lengthTests :: Test
lengthTests =
  TestLabel "length'" $
    TestList
      [ mkTest "returns 0 for an empty list" (length ([] :: [Int])) (length' ([] :: [Int]))
      , mkTest "counts a short list" (length [1 .. 5 :: Int]) (length' ([1 .. 5] :: [Int]))
      , mkTest "works for strings" (length "kitimark") (length' "kitimark")
      , mkTest "handles nested lists" (length ([[1], [2], [3]] :: [[Int]])) (length' ([[1], [2], [3]] :: [[Int]]))
      ]

groupTests :: Test
groupTests =
  TestLabel "group'" $
    TestList
      [ mkTest "groups empty list" (group ([] :: [Int])) (group' ([] :: [Int]))
      , mkTest "groups single element" (group [1 :: Int]) (group' [1 :: Int])
      , mkTest "groups identical run" (group [1,1,1 :: Int]) (group' [1,1,1 :: Int])
      , mkTest "groups alternating values" (group [1,1,2,2,1,1 :: Int]) (group' [1,1,2,2,1,1 :: Int])
      , mkTest "groups characters" (group "aaabbc") (group' "aaabbc")
      ]

filterTests :: Test
filterTests =
  TestLabel "filter'" $
    TestList
      [ mkTest "filters even numbers" (filter even [1 .. 10 :: Int]) (filter' even [1 .. 10 :: Int])
      , mkTest "filters characters" (filter (`elem` "aeiou") "kitimark") (filter' (`elem` "aeiou") "kitimark")
      , mkTest "returns empty when predicate never holds" (filter (> 10) [1 .. 5 :: Int]) (filter' (> 10) [1 .. 5 :: Int])
      , mkTest "works on empty list" (filter even ([] :: [Int])) (filter' even ([] :: [Int]))
      ]

headTests :: Test
headTests =
  TestLabel "head'" $
    TestList
      [ mkTest "picks the first element" (1 :: Int) (head' [1, 2, 3])
      , mkTest "works on strings" 'k' (head' "kitimark")
      , expectError "fails on empty list" (head ([] :: [Int])) (head' [] :: Int)
      ]

tailTests :: Test
tailTests =
  TestLabel "tail'" $
    TestList
      [ mkTest "drops the first element" ([2, 3] :: [Int]) (tail' [1, 2, 3])
      , mkTest "works on strings" "itimark" (tail' "kitimark")
      , expectError "fails on empty list" (tail ([] :: [Int])) (tail' [] :: [Int])
      ]

initTests :: Test
initTests =
  TestLabel "init'" $
    TestList
      [ mkTest "removes the final element" ([1, 2] :: [Int]) (init' [1, 2, 3])
      , mkTest "works for singleton" ([] :: [Char]) (init' "k")
      , expectError "fails on empty list" (init ([] :: [Int])) (init' [] :: [Int])
      ]

lastTests :: Test
lastTests =
  TestLabel "last'" $
    TestList
      [ mkTest "picks the final element" (3 :: Int) (last' [1, 2, 3])
      , mkTest "works on strings" 'k' (last' "kitimark")
      , expectError "fails on empty list" (last ([] :: [Int])) (last' [] :: Int)
      ]

reverseTests :: Test
reverseTests =
  TestLabel "reverse'" $
    TestList
      [ mkTest "leaves an empty list unchanged" (reverse ([] :: [Int])) (reverse' [])
      , mkTest "leaves a singleton untouched" (reverse [42 :: Int]) (reverse' [42])
      , mkTest "flips an ordered list" (reverse ([1,2,3,4,5] :: [Int])) (reverse' ([1,2,3,4,5] :: [Int]))
      , mkTest "reverses a string" (reverse "kitimark") (reverse' "kitimark")
      , let xs = replicate 20 'a'
         in mkTest "preserves list length" (length (reverse xs)) (length' (reverse' xs))
      , let xs = [True, False, False, True]
         in mkTest "is an involution" (reverse (reverse xs)) (reverse' (reverse' xs))
      ]

concatTests :: Test
concatTests =
  TestLabel "concat'" $
    TestList
      [ mkTest "concat empty list of lists" (concat ([] :: [[Int]])) (concat' [])
      , mkTest "concat nested integer lists" (concat [[1,2], [3], [4,5 :: Int]]) (concat' [[1,2], [3], [4,5 :: Int]])
      , mkTest "concat list of strings" (concat ["ki","ti","mark"]) (concat' ["ki","ti","mark"])
      , mkTest "concat includes empty sub lists" (concat [[1], [], [2,3 :: Int]]) (concat' [[1], [], [2,3 :: Int]])
      ]

mapTests :: Test
mapTests =
  TestLabel "map'" $
    TestList
      [ mkTest "maps over integers" (map (*2) [1 .. 5 :: Int]) (map' (*2) [1 .. 5 :: Int])
      , mkTest "maps over characters" (map toUpper "kitimark") (map' toUpper "kitimark")
      , mkTest "map on empty list" (map (\x -> x + 1) ([] :: [Int])) (map' (\x -> x + 1) ([] :: [Int]))
      , mkTest "maps with boolean predicate" (map even [1,2,3,4 :: Int]) (map' even [1,2,3,4 :: Int])
      ]

concatMapTests :: Test
concatMapTests =
  TestLabel "concatMap'" $
    TestList
      [ mkTest "concatMap doubles each element" (concatMap (\x -> [x,x]) [1,2,3 :: Int]) (concatMap' (\x -> [x,x]) [1,2,3 :: Int])
      , mkTest "concatMap empty list" (concatMap (\_ -> [1 :: Int]) []) (concatMap' (\_ -> [1 :: Int]) [])
      , mkTest "concatMap with strings" (concatMap (\c -> [c,'-']) "kit") (concatMap' (\c -> [c,'-']) "kit")
      , mkTest "concatMap with mixed sublists" (concatMap (\xs -> xs ++ [0]) [[1,2],[3 :: Int]]) (concatMap' (\xs -> xs ++ [0]) [[1,2],[3 :: Int]])
      ]

anyTests :: Test
anyTests =
  TestLabel "any'" $
    TestList
      [ mkTest "is False for empty list" (any even ([] :: [Int])) (any' even ([] :: [Int]))
      , mkTest "detects predicate match" (any even [1,3,4,5 :: Int]) (any' even [1,3,4,5 :: Int])
      , mkTest "returns False when predicate never holds" (any (> 10) [1 .. 5 :: Int]) (any' (> 10) [1 .. 5 :: Int])
      , mkTest "works with characters" (any (== 'i') "kitimark") (any' (== 'i') "kitimark")
      , let xs = [False, False, True, False]
         in mkTest "mimics Prelude any" (any id xs) (any' id xs)
      ]

allPrimeTests :: Test
allPrimeTests =
  TestLabel "all'" $
    TestList
      [ mkTest "is True for empty list" (all even ([] :: [Int])) (all' even ([] :: [Int]))
      , mkTest "verifies predicate for positives" (all even [2,4,6,8 :: Int]) (all' even [2,4,6,8 :: Int])
      , mkTest "detects failure" (all even [2,3,4 :: Int]) (all' even [2,3,4 :: Int])
      , mkTest "works on characters" (all (== 'k') "kkkk") (all' (== 'k') "kkkk")
      , mkTest "returns False when any predicate fails" (all (> 0) [1,0,2 :: Int]) (all' (> 0) [1,0,2 :: Int])
      ]

andTests :: Test
andTests =
  TestLabel "and'" $
    TestList
      [ mkTest "True for empty list" (and []) (and' [])
      , mkTest "propagates True for all True list" (and [True, True, True]) (and' [True, True, True])
      , mkTest "stops at first False" (and [True, False, True]) (and' [True, False, True])
      , mkTest "works with singleton False" (and [False]) (and' [False])
      ]

orTests :: Test
orTests =
  TestLabel "or'" $
    TestList
      [ mkTest "False for empty list" (or []) (or' [])
      , mkTest "detects True anywhere" (or [False, False, True, False]) (or' [False, False, True, False])
      , mkTest "stays False when all False" (or [False, False]) (or' [False, False])
      , mkTest "works with singleton True" (or [True]) (or' [True])
      ]

sumTests :: Test
sumTests =
  TestLabel "sum'" $
    TestList
      [ mkTest "sums an empty list to 0" (sum ([] :: [Int])) (sum' [])
      , mkTest "sums a consecutive range" (sum [1 .. 5 :: Int]) (sum' [1 .. 5])
      , mkTest "works with negative numbers" (sum [1, -4, -3, 0 :: Int]) (sum' [1, -4, -3, 0])
      , let xs = replicate 4 7
         in mkTest "matches repeated addition" (sum xs) (sum' xs)
      ]

productTests :: Test
productTests =
  TestLabel "product'" $
    TestList
      [ mkTest "empty list multiplies to 1" (product ([] :: [Int])) (product' [])
      , mkTest "multiplies small list" (product [1 .. 5 :: Int]) (product' [1 .. 5])
      , mkTest "handles negatives" (product [1, -2, 3, 4 :: Int]) (product' [1, -2, 3, 4])
      , let xs = replicate 3 (2 :: Int)
         in mkTest "matches exponentiation for repeated factors" (product xs) (product' xs)
      ]

maximumTests :: Test
maximumTests =
  TestLabel "maximum'" $
    TestList
      [ mkTest "single element is its own max" (42 :: Int) (maximum' [42])
      , mkTest "finds max in ascending list" (5 :: Int) (maximum' [1 .. 5])
      , mkTest "finds max in descending list" (5 :: Int) (maximum' [5,4,3,2,1])
      , mkTest "works with negatives" (-1 :: Int) (maximum' [-3,-10,-1,-7])
      , mkTest "handles duplicates" (3 :: Int) (maximum' [3,1,3,2])
      , expectError "fails on empty list" (maximum ([] :: [Int])) (maximum' [] :: Int)
      ]

minimumTests :: Test
minimumTests =
  TestLabel "minimum'" $
    TestList
      [ mkTest "single element is its own min" (minimum [42 :: Int]) (minimum' [42 :: Int])
      , mkTest "finds min in ascending list" (minimum [1 .. 5 :: Int]) (minimum' [1 .. 5 :: Int])
      , mkTest "finds min in descending list" (minimum [5,4,3,2,1 :: Int]) (minimum' [5,4,3,2,1 :: Int])
      , mkTest "works with negatives" (minimum [-3,-10,-1,-7 :: Int]) (minimum' [-3,-10,-1,-7 :: Int])
      , mkTest "handles duplicates" (minimum [3,1,3,2 :: Int]) (minimum' [3,1,3,2 :: Int])
      , expectError "fails on empty list" (minimum ([] :: [Int])) (minimum' [] :: Int)
      ]

iterateTests :: Test
iterateTests =
  TestLabel "iterate''" $
    TestList
      [ mkTest "produces first few squares" (take 5 (iterate (^2) 2 :: [Int])) (take 5 (iterate'' (^2) 2 :: [Int]))
      , mkTest "produces arithmetic sequence" (take 6 (iterate (+3) 0 :: [Int])) (take 6 (iterate'' (+3) 0 :: [Int]))
      , mkTest "works with booleans" (take 4 (iterate not True)) (take 4 (iterate'' not True))
      ]

repeatTests :: Test
repeatTests =
  TestLabel "repeat'" $
    TestList
      [ mkTest "produces repeated ints" (take 5 (repeat 7)) (take 5 (repeat' 7))
      , mkTest "works with characters" (take 3 (repeat 'k')) (take 3 (repeat' 'k'))
      , mkTest "head yields the repeated value" (and $ take 4 $ map (== 42) (repeat 42)) (and $ take 4 $ map (== 42) (repeat' 42))
      ]

replicateTests :: Test
replicateTests =
  TestLabel "replicate'" $
    TestList
      [ mkTest "replicates integers" (replicate 5 (2 :: Int)) (replicate' 5 (2 :: Int))
      , mkTest "replicates characters" (replicate 3 'k') (replicate' 3 'k')
      , mkTest "replicate zero gives empty" (replicate 0 True) (replicate' 0 True)
      ]

cycleTests :: Test
cycleTests =
  TestLabel "cycle'" $
    TestList
      [ mkTest "cycle repeating integers" (take 8 (cycle [1,2,3 :: Int])) (take 8 (cycle' [1,2,3 :: Int]))
      , mkTest "cycle repeating characters" (take 10 (cycle "kit")) (take 10 (cycle' "kit"))
      , mkTest "cycle singleton list" (take 5 (cycle [True])) (take 5 (cycle' [True]))
      , expectError "cycle fails on empty list" (cycle ([] :: [Int])) (cycle' ([] :: [Int]))
      ]

takeTests :: Test
takeTests =
  TestLabel "take'" $
    TestList
      [ mkTest "take 0 yields empty list" (take 0 [1 .. 10 :: Int]) (take' 0 [1 .. 10])
      , mkTest "take n from short list" (take 3 [1 .. 5 :: Int]) (take' 3 [1 .. 5])
      , mkTest "take larger than list length returns full list" (take 5 [1,2,3 :: Int]) (take' 5 [1,2,3])
      , mkTest "take from infinite list" (take 4 (repeat 'k')) (take' 4 (repeat' 'k'))
      ]

dropTests :: Test
dropTests =
  TestLabel "drop'" $
    TestList
      [ mkTest "drop 0 returns original list" (drop 0 [1,2,3 :: Int]) (drop' 0 [1,2,3])
      , mkTest "drop exact length yields empty list" (drop 3 [1,2,3 :: Int]) (drop' 3 [1,2,3])
      , mkTest "drop more than length yields empty list" (drop 5 [1,2,3 :: Int]) (drop' 5 [1,2,3])
      , mkTest "drop from infinite list" (take 3 (drop 5 (repeat 'k'))) (take 3 (drop' 5 (repeat' 'k')))
      ]

takeWhileTests :: Test
takeWhileTests =
  TestLabel "takeWhile'" $
    TestList
      [ mkTest "empty list stays empty" (takeWhile even ([] :: [Int])) (takeWhile' even [])
      , mkTest "takes prefix while predicate holds" (takeWhile even [2,4,5,6 :: Int]) (takeWhile' even [2,4,5,6])
      , mkTest "returns full list if predicate always True" (takeWhile (== 'a') "aaa") (takeWhile' (== 'a') "aaa")
      , mkTest "works with infinite list" (takeWhile (< 3) (take 5 (repeat 2))) (takeWhile' (< 3) (take 5 (repeat' 2)))
      ]

dropWhileTests :: Test
dropWhileTests =
  TestLabel "dropWhile'" $
    TestList
      [ mkTest "empty list stays empty" (dropWhile even ([] :: [Int])) (dropWhile' even [])
      , mkTest "drops prefix until predicate fails" (dropWhile even [2,4,5,6 :: Int]) (dropWhile' even [2,4,5,6])
      , mkTest "drops entire list when predicate always True" (dropWhile (== 'a') "aaa") (dropWhile' (== 'a') "aaa")
      , mkTest "works with infinite list" (take 5 (dropWhile (< 3) (repeat 3))) (take 5 (dropWhile' (< 3) (repeat' 3)))
      ]

spanTests :: Test
spanTests =
  TestLabel "span'" $
    TestList
      [ mkTest "empty list returns empty pair" (span even ([] :: [Int])) (span' even [])
      , mkTest "splits when predicate fails" (span even [2,4,5,6 :: Int]) (span' even [2,4,5,6])
      , mkTest "all elements satisfy predicate" (span (== 'a') "aaa") (span' (== 'a') "aaa")
      , mkTest "no elements satisfy predicate" (span (== 'a') "bbb") (span' (== 'a') "bbb")
      ]

elemTests :: Test
elemTests =
  TestLabel "elem'" $
    TestList
      [ mkTest "finds integer in list" (elem 3 [1,2,3,4 :: Int]) (elem' 3 [1,2,3,4 :: Int])
      , mkTest "returns False when missing" (elem 5 [1,2,3 :: Int]) (elem' 5 [1,2,3 :: Int])
      , mkTest "works on characters" (elem 'k' "kitimark") (elem' 'k' "kitimark")
      , mkTest "False for empty list" (elem True ([] :: [Bool])) (elem' True ([] :: [Bool]))
      ]

zipTests :: Test
zipTests =
  TestLabel "zip'" $
    TestList
      [ mkTest "zips equal-length lists" (zip [1..3] ['a'..'c']) (zip' [1,2,3] ['a','b','c'])
      , mkTest "stops when first list empty" ([] :: [(Int, Char)]) (zip' [] ['a','b'])
      , mkTest "stops when second list empty" ([] :: [(Int, Char)]) (zip' [1,2] [])
      , mkTest "zips uneven lists to shorter length" (zip [1,2] [True, False]) (zip' [1,2,3] [True, False])
      ]

unzipTests :: Test
unzipTests =
  TestLabel "unzip'" $
    TestList
      [ mkTest "unzip empty list" (unzip ([] :: [(Int, Char)])) (unzip' [])
      , mkTest "unzip single pair" (unzip [(1, 'a')]) (unzip' [(1, 'a')])
      , mkTest "unzip multiple pairs" (unzip [(1,'a'), (2,'b'), (3,'c')]) (unzip' [(1,'a'), (2,'b'), (3,'c')])
      ]

deleteTests :: Test
deleteTests =
  TestLabel "delete'" $
    TestList
      [ mkTest "removes first occurrence" (delete 3 [1,2,3,4 :: Int]) (delete' 3 [1,2,3,4 :: Int])
      , mkTest "removes head element" (delete 1 [1,1,2 :: Int]) (delete' 1 [1,1,2 :: Int])
      , mkTest "leaves list unchanged when element missing" (delete 5 [1,2,3,4 :: Int]) (delete' 5 [1,2,3,4 :: Int])
      , mkTest "works on characters" (delete 'k' "kitimark") (delete' 'k' "kitimark")
      ]

intersectTests :: Test
intersectTests =
  TestLabel "intersect'" $
    TestList
      [ mkTest "intersect empty with list" (intersect [] [1,2,3 :: Int]) (intersect' [] [1,2,3 :: Int])
      , mkTest "intersect list with empty" (intersect [1,2,3 :: Int] []) (intersect' [1,2,3 :: Int] [])
      , mkTest "intersect integers keeps order of first" (intersect [1,2,3,4 :: Int] [3,4,5]) (intersect' [1,2,3,4 :: Int] [3,4,5])
      , mkTest "intersect duplicates" (intersect [1,1,2,3 :: Int] [1,2,2,3]) (intersect' [1,1,2,3 :: Int] [1,2,2,3])
      , mkTest "intersect strings" (intersect "kitimark" "matrix") (intersect' "kitimark" "matrix")
      ]

intersperseTests :: Test
intersperseTests =
  TestLabel "intersperse'" $
    TestList
      [ mkTest "empty list stays empty" (intersperse 0 ([] :: [Int])) (intersperse' 0 ([] :: [Int]))
      , mkTest "singleton list unaffected" (intersperse 0 [42 :: Int]) (intersperse' 0 [42 :: Int])
      , mkTest "inserts separators between numbers" (intersperse 0 [1,2,3 :: Int]) (intersperse' 0 [1,2,3 :: Int])
      , mkTest "works on strings" (intersperse '-' "kitimark") (intersperse' '-' "kitimark")
      ]

intercalateTests :: Test
intercalateTests =
  TestLabel "intercalate'" $
    TestList
      [ mkTest "empty list stays empty" (intercalate ([] :: [Int]) []) (intercalate' ([] :: [Int]) [])
      , mkTest "single sublist unaffected" (intercalate [0 :: Int] [[1,2,3]]) (intercalate' [0 :: Int] [[1,2,3]])
      , mkTest "joins sublists with separator" (intercalate [0] [[1,2],[3],[4,5 :: Int]]) (intercalate' [0] [[1,2],[3],[4,5 :: Int]])
      , mkTest "intercalates strings" (intercalate "-" ["ki","ti","mark"]) (intercalate' "-" ["ki","ti","mark"])
      , mkTest "handles empty separators" (intercalate "" ["kit","im","ark"]) (intercalate' "" ["kit","im","ark"])
      ]

insertionsortTests :: Test
insertionsortTests =
  TestLabel "insertionsort" $
    TestList
      [ mkTest "sorts empty list" (sort ([] :: [Int])) (insertionsort ([] :: [Int]))
      , mkTest "sorts already sorted list" (sort [1,2,3,4 :: Int]) (insertionsort [1,2,3,4 :: Int])
      , mkTest "sorts reverse list" (sort [5,4,3,2,1 :: Int]) (insertionsort [5,4,3,2,1 :: Int])
      , mkTest "handles duplicates" (sort [3,1,2,3,2 :: Int]) (insertionsort [3,1,2,3,2 :: Int])
      , mkTest "sorts characters" (sort "kitimark") (insertionsort "kitimark")
      ]

insertTests :: Test
insertTests =
  TestLabel "insert'" $
    TestList
      [ mkTest "inserts into empty list" (insert 3 ([] :: [Int])) (insert' 3 ([] :: [Int]))
      , mkTest "inserts at head" (insert 0 [1,2,3 :: Int]) (insert' 0 [1,2,3 :: Int])
      , mkTest "inserts in middle" (insert 3 [1,2,4,5 :: Int]) (insert' 3 [1,2,4,5 :: Int])
      , mkTest "inserts at end" (insert 5 [1,2,3,4 :: Int]) (insert' 5 [1,2,3,4 :: Int])
      , mkTest "handles duplicates" (insert 2 [1,2,2,3 :: Int]) (insert' 2 [1,2,2,3 :: Int])
      , mkTest "works with characters" (insert 'a' "kitimrk") (insert' 'a' "kitimrk")
      ]

mergesortTests :: Test
mergesortTests =
  TestLabel "mergesort" $
    TestList
      [ mkTest "sorts empty list" (sort ([] :: [Int])) (mergesort ([] :: [Int]))
      , mkTest "sorts singleton list" (sort [42 :: Int]) (mergesort [42 :: Int])
      , mkTest "sorts reverse list" (sort [5,4,3,2,1 :: Int]) (mergesort [5,4,3,2,1 :: Int])
      , mkTest "handles duplicates" (sort [3,1,2,3,2 :: Int]) (mergesort [3,1,2,3,2 :: Int])
      , mkTest "sorts characters" (sort "kitimark") (mergesort "kitimark")
      ]

mergeTests :: Test
mergeTests =
  TestLabel "merge" $
    TestList
      [ mkTest "merges two empty lists" (sort ([] :: [Int])) (merge ([] :: [Int]) [])
      , mkTest "merges empty with singleton" (sort [1 :: Int]) (merge [] [1 :: Int])
      , mkTest "merges singleton with empty" (sort [1 :: Int]) (merge [1 :: Int] [])
      , mkTest "merges multi-element with empty" (sort [1,2,3 :: Int]) (merge [1,2,3 :: Int] [])
      , mkTest "merges unequal lengths" (sort ([1,2,3 :: Int] ++ [4,5 :: Int])) (merge [1,2,3 :: Int] [4,5 :: Int])
      , mkTest "merges alternating lists" (sort ([1,3,5 :: Int] ++ [2,4,6 :: Int])) (merge [1,3,5 :: Int] [2,4,6 :: Int])
      , mkTest "merges with duplicates" (sort ([1,1,3 :: Int] ++ [1,2,2 :: Int])) (merge [1,1,3 :: Int] [1,2,2 :: Int])
      , mkTest "merges characters" (sort ("ace" ++ "bdf")) (merge "ace" "bdf")
      ]

stalinSortTests :: Test
stalinSortTests =
  TestLabel "stalinSort" $
    TestList
      [ mkTest "sorts empty list" ([] :: [Int]) (stalinSort [] :: [Int])
      , mkTest "sorts singleton list" [1 :: Int] (stalinSort [1 :: Int])
      , mkTest "sorts two elements sorted list" [1, 2 :: Int] (stalinSort [1, 2 :: Int])
      , mkTest "sorts two elements un-sorted list" [2 :: Int] (stalinSort [2, 1 :: Int])
      , mkTest "sorts from homework example 1 element" [4] (stalinSort [4 :: Int])
      , mkTest "sorts from homework example 2 elements" [2, 4] (stalinSort [2, 4 :: Int])
      , mkTest "sorts from homework example 3 elements" [11] (stalinSort [11, 2, 4 :: Int])
      , mkTest "sorts from homework example 4 elements" [10, 11] (stalinSort [10, 11, 2, 4 :: Int])
      , mkTest "sorts from homework example 5 elements" [9, 10, 11] (stalinSort [9, 10, 11, 2, 4 :: Int])
      , mkTest "sorts from homework example 6 elements" [1, 9, 10, 11] (stalinSort [1, 9, 10, 11, 2, 4 :: Int])
      , mkTest "sorts from homework example 7 elements" [7, 9, 10, 11] (stalinSort [7, 1, 9, 10, 11, 2, 4 :: Int])
      , mkTest "sorts from homework example 8 elements" [3, 7, 9, 10, 11] (stalinSort [3, 7, 1, 9, 10, 11, 2, 4 :: Int])
      , mkTest "sorts from homework example 9 elements" [4, 7, 9, 10, 11] (stalinSort [4, 3, 7, 1, 9, 10, 11, 2, 4 :: Int])
      , mkTest "sorts from homework example 10 elements" [2, 4, 7, 9, 10, 11] (stalinSort [2, 4, 3, 7, 1, 9, 10, 11, 2, 4 :: Int])
      , mkTest "sorts from homework example 11 elements" [1, 2, 4, 7, 9, 10, 11] (stalinSort [1, 2, 4, 3, 7, 1, 9, 10, 11, 2, 4 :: Int])
      , mkTest "sorts from homework example" [3, 4, 7, 9, 10, 11] (stalinSort [3, 1, 2, 4, 3, 7, 1, 9, 10, 11, 2, 4 :: Int])
      ]

allTests :: Test
allTests = TestList
  [ lengthTests
  , groupTests
  , filterTests
  , headTests
  , tailTests
  , initTests
  , lastTests
  , reverseTests
  , concatTests
  , mapTests
  , concatMapTests
  , anyTests
  , allPrimeTests
  , andTests
  , orTests
  , sumTests
  , productTests
  , maximumTests
  , minimumTests
  , iterateTests
  , repeatTests
  , replicateTests
  , cycleTests
  , takeTests
  , dropTests
  , takeWhileTests
  , dropWhileTests
  , spanTests
  , elemTests
  , zipTests
  , unzipTests
  , deleteTests
  , intersectTests
  , intersperseTests
  , intercalateTests
  , insertionsortTests
  , insertTests
  , mergesortTests
  , mergeTests
  , stalinSortTests
  ]

main :: IO ()
main = do
  counts <- runTestTT allTests
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
