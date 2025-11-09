{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Char (isUpper, toUpper)
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
      [ mkTest "returns 0 for an empty list" 0 (length' ([] :: [Int]))
      , mkTest "counts a short list" 5 (length' ([1 .. 5] :: [Int]))
      , mkTest "works for strings" 8 (length' "kitimark")
      , mkTest "handles nested lists" 3 (length' ([[1], [2], [3]] :: [[Int]]))
      , mkTest "comparison of length function - returns 0 for an empty list" (length ([] :: [Int])) (length' ([] :: [Int]))
      , mkTest "comparison of length function - counts a short list" (length [1 .. 5 :: Int]) (length' ([1 .. 5] :: [Int]))
      , mkTest "comparison of length function - works for strings" (length "kitimark") (length' "kitimark")
      , mkTest "comparison of length function - handles nested lists" (length ([[1], [2], [3]] :: [[Int]])) (length' ([[1], [2], [3]] :: [[Int]]))
      ]

groupTests :: Test
groupTests =
  TestLabel "group'" $
    TestList
      [ mkTest "groups empty list" ([] :: [[Int]]) (group' ([] :: [Int]))
      , mkTest "groups single element" ([[1]] :: [[Int]]) (group' [1 :: Int])
      , mkTest "groups identical run" ([[1,1,1]] :: [[Int]]) (group' [1,1,1 :: Int])
      , mkTest "groups alternating values" ([[1,1],[2,2],[1,1]] :: [[Int]]) (group' [1,1,2,2,1,1 :: Int])
      , mkTest "groups characters" ["aaa","bb","c"] (group' "aaabbc")
      , mkTest "comparison of group function - handles mixed runs" (group [1,2,2,3,3,3 :: Int]) (group' [1,2,2,3,3,3 :: Int])
      , mkTest "comparison of group function - works on words" (group "mississippi") (group' "mississippi")
      ]

nubTests :: Test
nubTests =
  TestLabel "nub'" $
    TestList
      [ mkTest "handles empty list" ([] :: [Int]) (nub' ([] :: [Int]))
      , mkTest "drops duplicate ints" ([1,2,3,4] :: [Int]) (nub' [1,1,2,2,3,4,3 :: Int])
      , mkTest "preserves first occurrence order" "kitmar" (nub' "kitimark")
      , mkTest "works with booleans" [True, False] (nub' [True, True, False, True])
      , mkTest "comparison of nub function - numbers" (nub [1,2,1,3,2 :: Int]) (nub' [1,2,1,3,2 :: Int])
      , mkTest "comparison of nub function - strings" (nub "mississippi") (nub' "mississippi")
      ]

filterTests :: Test
filterTests =
  TestLabel "filter'" $
    TestList
      [ mkTest "filters even numbers" ([2,4,6,8,10] :: [Int]) (filter' even [1 .. 10 :: Int])
      , mkTest "filters characters" "iia" (filter' (`elem` "aeiou") "kitimark")
      , mkTest "returns empty when predicate never holds" ([] :: [Int]) (filter' (> 10) [1 .. 5 :: Int])
      , mkTest "works on empty list" ([] :: [Int]) (filter' even ([] :: [Int]))
      , mkTest "comparison of filter function - keeps odds" (filter odd [1 .. 7 :: Int]) (filter' odd [1 .. 7 :: Int])
      , mkTest "comparison of filter function - removes consonants" (filter (`elem` "aeiou") "functional") (filter' (`elem` "aeiou") "functional")
      ]

headTests :: Test
headTests =
  TestLabel "head'" $
    TestList
      [ mkTest "picks the first element" (1 :: Int) (head' [1, 2, 3])
      , mkTest "works on strings" 'k' (head' "kitimark")
      , expectError "fails on empty list" (head ([] :: [Int])) (head' [] :: Int)
      , mkTest "comparison of head function - picks the first element" (head [1, 2, 3 :: Int]) (head' [1, 2, 3 :: Int])
      , mkTest "comparison of head function - works on strings" (head "kitimark") (head' "kitimark")
      ]

tailTests :: Test
tailTests =
  TestLabel "tail'" $
    TestList
      [ mkTest "drops the first element" ([2, 3] :: [Int]) (tail' [1, 2, 3])
      , mkTest "works on strings" "itimark" (tail' "kitimark")
      , expectError "fails on empty list" (tail ([] :: [Int])) (tail' [] :: [Int])
      , mkTest "comparison of tail function - drops the first element" (tail [1, 2, 3 :: Int]) (tail' [1, 2, 3 :: Int])
      , mkTest "comparison of tail function - works on strings" (tail "kitimark") (tail' "kitimark")
      ]

initTests :: Test
initTests =
  TestLabel "init'" $
    TestList
      [ mkTest "removes the final element" ([1, 2] :: [Int]) (init' [1, 2, 3])
      , mkTest "works for singleton" ([] :: [Char]) (init' "k")
      , expectError "fails on empty list" (init ([] :: [Int])) (init' [] :: [Int])
      , mkTest "comparison of init function - removes the final element" (init [1, 2, 3 :: Int]) (init' [1, 2, 3 :: Int])
      , mkTest "comparison of init function - works for singleton" (init "k") (init' "k")
      ]

lastTests :: Test
lastTests =
  TestLabel "last'" $
    TestList
      [ mkTest "picks the final element" (3 :: Int) (last' [1, 2, 3])
      , mkTest "works on strings" 'k' (last' "kitimark")
      , expectError "fails on empty list" (last ([] :: [Int])) (last' [] :: Int)
      , mkTest "comparison of last function - picks the final element" (last [1, 2, 3 :: Int]) (last' [1, 2, 3 :: Int])
      , mkTest "comparison of last function - works on strings" (last "kitimark") (last' "kitimark")
      ]

reverseTests :: Test
reverseTests =
  TestLabel "reverse'" $
    TestList
      [ mkTest "leaves an empty list unchanged" ([] :: [Int]) (reverse' [])
      , mkTest "leaves a singleton untouched" ([42] :: [Int]) (reverse' [42])
      , mkTest "flips an ordered list" ([5,4,3,2,1] :: [Int]) (reverse' ([1,2,3,4,5] :: [Int]))
      , mkTest "reverses a string" "kramitik" (reverse' "kitimark")
      , let xs = replicate 20 'a'
         in mkTest "preserves list length" 20 (length' (reverse' xs))
      , let xs = [True, False, False, True]
         in mkTest "is an involution" xs (reverse' (reverse' xs))
      , mkTest "comparison of reverse function - reverses nested lists" (reverse ([[1,2],[3,4],[5,6]] :: [[Int]])) (reverse' ([[1,2],[3,4],[5,6]] :: [[Int]]))
      , mkTest "comparison of reverse function - works on words" (reverse ["ki","ti","mark"]) (reverse' ["ki","ti","mark"])
      ]

concatTests :: Test
concatTests =
  TestLabel "concat'" $
    TestList
      [ mkTest "concat empty list of lists" ([] :: [Int]) (concat' [])
      , mkTest "concat nested integer lists" ([1,2,3,4,5] :: [Int]) (concat' [[1,2], [3], [4,5 :: Int]])
      , mkTest "concat list of strings" "kitimark" (concat' ["ki","ti","mark"])
      , mkTest "concat includes empty sub lists" ([1,2,3] :: [Int]) (concat' [[1], [], [2,3 :: Int]])
      , mkTest "comparison of concat function - mixes empties" (concat [[1,2], [], [3,4 :: Int]]) (concat' [[1,2], [], [3,4 :: Int]])
      , mkTest "comparison of concat function - handles characters" (concat ["ha","sk","ell"]) (concat' ["ha","sk","ell"])
      ]

mapTests :: Test
mapTests =
  TestLabel "map'" $
    TestList
      [ mkTest "maps over integers" ([2,4,6,8,10] :: [Int]) (map' (*2) [1 .. 5 :: Int])
      , mkTest "maps over characters" "KITIMARK" (map' toUpper "kitimark")
      , mkTest "map on empty list" ([] :: [Int]) (map' (\x -> x + 1) ([] :: [Int]))
      , mkTest "maps with boolean predicate" [False, True, False, True] (map' even [1,2,3,4 :: Int])
      , mkTest "comparison of map function - squares numbers" (map (^2) [0..4 :: Int]) (map' (^2) [0..4 :: Int])
      , mkTest "comparison of map function - appends punctuation" (map (++ "!") ["hi","there"]) (map' (++ "!") ["hi","there"])
      ]

concatMapTests :: Test
concatMapTests =
  TestLabel "concatMap'" $
    TestList
      [ mkTest "concatMap doubles each element" ([1,1,2,2,3,3] :: [Int]) (concatMap' (\x -> [x,x]) [1,2,3 :: Int])
      , mkTest "concatMap empty list" ([] :: [Int]) (concatMap' (\_ -> [1 :: Int]) [])
      , mkTest "concatMap with strings" "k-i-t-" (concatMap' (\c -> [c,'-']) "kit")
      , mkTest "concatMap with mixed sublists" ([1,2,0,3,0] :: [Int]) (concatMap' (\xs -> xs ++ [0]) [[1,2],[3 :: Int]])
      , mkTest "comparison of concatMap function - expands digits" (concatMap show [1,2,3 :: Int]) (concatMap' show [1,2,3 :: Int])
      , mkTest "comparison of concatMap function - duplicates chars" (concatMap (\c -> [c,c]) "ha") (concatMap' (\c -> [c,c]) "ha")
      ]

anyTests :: Test
anyTests =
  TestLabel "any'" $
    TestList
      [ mkTest "is False for empty list" False (any' even ([] :: [Int]))
      , mkTest "detects predicate match" True (any' even [1,3,4,5 :: Int])
      , mkTest "returns False when predicate never holds" False (any' (> 10) [1 .. 5 :: Int])
      , mkTest "works with characters" True (any' (== 'i') "kitimark")
      , let xs = [False, False, True, False]
         in mkTest "mimics Prelude any" True (any' id xs)
      , mkTest "comparison of any function - finds uppercase" (any isUpper "kitimark") (any' isUpper "kitimark")
      , mkTest "comparison of any function - checks positives" (any (>0) [-2,-1,0,1 :: Int]) (any' (>0) [-2,-1,0,1 :: Int])
      ]

allPrimeTests :: Test
allPrimeTests =
  TestLabel "all'" $
    TestList
      [ mkTest "is True for empty list" True (all' even ([] :: [Int]))
      , mkTest "verifies predicate for positives" True (all' even [2,4,6,8 :: Int])
      , mkTest "detects failure" False (all' even [2,3,4 :: Int])
      , mkTest "works on characters" True (all' (== 'k') "kkkk")
      , mkTest "returns False when any predicate fails" False (all' (> 0) [1,0,2 :: Int])
      , mkTest "comparison of all function - ensures uppercase" (all isUpper "KIT") (all' isUpper "KIT")
      , mkTest "comparison of all function - checks positives" (all (>0) [1,2,3 :: Int]) (all' (>0) [1,2,3 :: Int])
      ]

andTests :: Test
andTests =
  TestLabel "and'" $
    TestList
      [ mkTest "True for empty list" True (and' [])
      , mkTest "propagates True for all True list" True (and' [True, True, True])
      , mkTest "stops at first False" False (and' [True, False, True])
      , mkTest "works with singleton False" False (and' [False])
      , mkTest "comparison of and function - handles booleans" (and [True, True, False, True]) (and' [True, True, False, True])
      , mkTest "comparison of and function - only trues stay true" (and [True, True]) (and' [True, True])
      ]

orTests :: Test
orTests =
  TestLabel "or'" $
    TestList
      [ mkTest "False for empty list" False (or' [])
      , mkTest "detects True anywhere" True (or' [False, False, True, False])
      , mkTest "stays False when all False" False (or' [False, False])
      , mkTest "works with singleton True" True (or' [True])
      , mkTest "comparison of or function - mixed booleans" (or [False, False, False, True]) (or' [False, False, False, True])
      , mkTest "comparison of or function - stays false" (or [False, False, False]) (or' [False, False, False])
      ]

sumTests :: Test
sumTests =
  TestLabel "sum'" $
    TestList
      [ mkTest "sums an empty list to 0" 0 (sum' [])
      , mkTest "sums a consecutive range" 15 (sum' [1 .. 5])
      , mkTest "works with negative numbers" (-6) (sum' [1, -4, -3, 0])
      , let xs = replicate 4 7
         in mkTest "matches repeated addition" 28 (sum' xs)
      , mkTest "comparison of sum function - mixes ints" (sum [10,-5,3 :: Int]) (sum' [10,-5,3 :: Int])
      , mkTest "comparison of sum function - handles singleton" (sum [42 :: Int]) (sum' [42 :: Int])
      ]

productTests :: Test
productTests =
  TestLabel "product'" $
    TestList
      [ mkTest "empty list multiplies to 1" 1 (product' [])
      , mkTest "multiplies small list" 120 (product' [1 .. 5])
      , mkTest "handles negatives" (-24) (product' [1, -2, 3, 4])
      , let xs = replicate 3 (2 :: Int)
         in mkTest "matches exponentiation for repeated factors" 8 (product' xs)
      , mkTest "comparison of product function - mixes zeros" (product [0,1,2 :: Int]) (product' [0,1,2 :: Int])
      , mkTest "comparison of product function - singleton" (product [7 :: Int]) (product' [7 :: Int])
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
      , mkTest "comparison of maximum function - single element is its own max" (maximum [42 :: Int]) (maximum' [42 :: Int])
      , mkTest "comparison of maximum function - finds max in ascending list" (maximum [1 .. 5 :: Int]) (maximum' [1 .. 5 :: Int])
      , mkTest "comparison of maximum function - finds max in descending list" (maximum [5,4,3,2,1 :: Int]) (maximum' [5,4,3,2,1 :: Int])
      , mkTest "comparison of maximum function - works with negatives" (maximum [-3,-10,-1,-7 :: Int]) (maximum' [-3,-10,-1,-7 :: Int])
      , mkTest "comparison of maximum function - handles duplicates" (maximum [3,1,3,2 :: Int]) (maximum' [3,1,3,2 :: Int])
      ]

minimumTests :: Test
minimumTests =
  TestLabel "minimum'" $
    TestList
      [ mkTest "single element is its own min" (42 :: Int) (minimum' [42 :: Int])
      , mkTest "finds min in ascending list" (1 :: Int) (minimum' [1 .. 5 :: Int])
      , mkTest "finds min in descending list" (1 :: Int) (minimum' [5,4,3,2,1 :: Int])
      , mkTest "works with negatives" (-10 :: Int) (minimum' [-3,-10,-1,-7 :: Int])
      , mkTest "handles duplicates" (1 :: Int) (minimum' [3,1,3,2 :: Int])
      , expectError "fails on empty list" (minimum ([] :: [Int])) (minimum' [] :: Int)
      , mkTest "comparison of minimum function - singleton" (minimum [5 :: Int]) (minimum' [5 :: Int])
      , mkTest "comparison of minimum function - mixed signs" (minimum [3,-1,4,-2 :: Int]) (minimum' [3,-1,4,-2 :: Int])
      ]

iterateTests :: Test
iterateTests =
  TestLabel "iterate''" $
    TestList
      [ mkTest "produces first few squares" ([2,4,16,256,65536] :: [Int]) (take 5 (iterate'' (^2) 2 :: [Int]))
      , mkTest "produces arithmetic sequence" ([0,3,6,9,12,15] :: [Int]) (take 6 (iterate'' (+3) 0 :: [Int]))
      , mkTest "works with booleans" [True, False, True, False] (take 4 (iterate'' not True))
      , mkTest "comparison of iterate function - doubles values" (take 5 (iterate (*2) 1 :: [Int])) (take 5 (iterate'' (*2) 1 :: [Int]))
      , mkTest "comparison of iterate function - decrements" (take 4 (iterate (subtract 1) 0 :: [Int])) (take 4 (iterate'' (subtract 1) 0 :: [Int]))
      ]

repeatTests :: Test
repeatTests =
  TestLabel "repeat'" $
    TestList
      [ mkTest "produces repeated ints" ([7,7,7,7,7] :: [Int]) (take 5 (repeat' 7))
      , mkTest "works with characters" "kkk" (take 3 (repeat' 'k'))
      , mkTest "head yields the repeated value" True (and $ take 4 $ map (== 42) (repeat' 42))
      , mkTest "comparison of repeat function - booleans" (take 6 (repeat True)) (take 6 (repeat' True))
      , mkTest "comparison of repeat function - tuples" (take 4 (repeat (1,'a'))) (take 4 (repeat' (1,'a')))
      ]

replicateTests :: Test
replicateTests =
  TestLabel "replicate'" $
    TestList
      [ mkTest "replicates integers" ([2,2,2,2,2] :: [Int]) (replicate' 5 (2 :: Int))
      , mkTest "replicates characters" "kkk" (replicate' 3 'k')
      , mkTest "replicate zero gives empty" ([] :: [Bool]) (replicate' 0 True)
      , mkTest "comparison of replicate function - booleans" (replicate 4 False) (replicate' 4 False)
      , mkTest "comparison of replicate function - strings" (replicate 2 "ki") (replicate' 2 "ki")
      ]

cycleTests :: Test
cycleTests =
  TestLabel "cycle'" $
    TestList
      [ mkTest "cycle repeating integers" ([1,2,3,1,2,3,1,2] :: [Int]) (take 8 (cycle' [1,2,3 :: Int]))
      , mkTest "cycle repeating characters" "kitkitkitk" (take 10 (cycle' "kit"))
      , mkTest "cycle singleton list" [True, True, True, True, True] (take 5 (cycle' [True]))
      , expectError "cycle fails on empty list" (cycle ([] :: [Int])) (cycle' ([] :: [Int]))
      , mkTest "comparison of cycle function - tuples" (take 6 (cycle [(1,'a'),(2,'b')])) (take 6 (cycle' [(1,'a'),(2,'b')]))
      , mkTest "comparison of cycle function - booleans" (take 7 (cycle [True, False])) (take 7 (cycle' [True, False]))
      ]

takeTests :: Test
takeTests =
  TestLabel "take'" $
    TestList
      [ mkTest "take 0 yields empty list" ([] :: [Int]) (take' 0 [1 .. 10])
      , mkTest "take n from short list" ([1,2,3] :: [Int]) (take' 3 [1 .. 5])
      , mkTest "take larger than list length returns full list" ([1,2,3] :: [Int]) (take' 5 [1,2,3])
      , mkTest "take from infinite list" "kkkk" (take' 4 (repeat' 'k'))
      , mkTest "comparison of take function - strings" (take 2 "kitimark") (take' 2 "kitimark")
      , mkTest "comparison of take function - zero elements" (take 0 ([] :: [Int])) (take' 0 ([] :: [Int]))
      ]

dropTests :: Test
dropTests =
  TestLabel "drop'" $
    TestList
      [ mkTest "drop 0 returns original list" ([1,2,3] :: [Int]) (drop' 0 [1,2,3])
      , mkTest "drop exact length yields empty list" ([] :: [Int]) (drop' 3 [1,2,3])
      , mkTest "drop more than length yields empty list" ([] :: [Int]) (drop' 5 [1,2,3])
      , mkTest "drop from infinite list" "kkk" (take 3 (drop' 5 (repeat' 'k')))
      , mkTest "comparison of drop function - strings" (drop 2 "kitimark") (drop' 2 "kitimark")
      , mkTest "comparison of drop function - zero" (drop 0 ([] :: [Int])) (drop' 0 ([] :: [Int]))
      ]

takeWhileTests :: Test
takeWhileTests =
  TestLabel "takeWhile'" $
    TestList
      [ mkTest "empty list stays empty" ([] :: [Int]) (takeWhile' even [])
      , mkTest "takes prefix while predicate holds" ([2,4] :: [Int]) (takeWhile' even [2,4,5,6 :: Int])
      , mkTest "returns full list if predicate always True" "aaa" (takeWhile' (== 'a') "aaa")
      , mkTest "works with infinite list" ([2,2,2,2,2] :: [Int]) (takeWhile' (< 3) (take 5 (repeat' 2)))
      , mkTest "comparison of takeWhile function - stops at odd" (takeWhile even [2,4,6,7,8 :: Int]) (takeWhile' even [2,4,6,7,8 :: Int])
      , mkTest "comparison of takeWhile function - handles chars" (takeWhile (/= 't') "kitimark") (takeWhile' (/= 't') "kitimark")
      ]

dropWhileTests :: Test
dropWhileTests =
  TestLabel "dropWhile'" $
    TestList
      [ mkTest "empty list stays empty" ([] :: [Int]) (dropWhile' even [])
      , mkTest "drops prefix until predicate fails" ([5,6] :: [Int]) (dropWhile' even [2,4,5,6 :: Int])
      , mkTest "drops entire list when predicate always True" "" (dropWhile' (== 'a') "aaa")
      , mkTest "works with infinite list" ([3,3,3,3,3] :: [Int]) (take 5 (dropWhile' (< 3) (repeat' 3)))
      , mkTest "comparison of dropWhile function - remove odds" (dropWhile odd [1,3,5,6,7 :: Int]) (dropWhile' odd [1,3,5,6,7 :: Int])
      , mkTest "comparison of dropWhile function - handles strings" (dropWhile (/= 'm') "kitimark") (dropWhile' (/= 'm') "kitimark")
      ]

spanTests :: Test
spanTests =
  TestLabel "span'" $
    TestList
      [ mkTest "empty list returns empty pair" (([], []) :: ([Int], [Int])) (span' even [])
      , mkTest "splits when predicate fails" (([2,4], [5,6]) :: ([Int], [Int])) (span' even [2,4,5,6 :: Int])
      , mkTest "all elements satisfy predicate" ("aaa", "") (span' (== 'a') "aaa")
      , mkTest "no elements satisfy predicate" ("", "bbb") (span' (== 'a') "bbb")
      , mkTest "comparison of span function - mixed ints" (span (<5) [1,2,5,6 :: Int]) (span' (<5) [1,2,5,6 :: Int])
      , mkTest "comparison of span function - strings" (span (/= 'i') "kitimark") (span' (/= 'i') "kitimark")
      ]

elemTests :: Test
elemTests =
  TestLabel "elem'" $
    TestList
      [ mkTest "finds integer in list" True (elem' 3 [1,2,3,4 :: Int])
      , mkTest "returns False when missing" False (elem' 5 [1,2,3 :: Int])
      , mkTest "works on characters" True (elem' 'k' "kitimark")
      , mkTest "False for empty list" False (elem' True ([] :: [Bool]))
      , mkTest "comparison of elem function - booleans" (elem False [True, False]) (elem' False [True, False])
      , mkTest "comparison of elem function - strings" (elem 'm' "kitimark") (elem' 'm' "kitimark")
      ]

zipTests :: Test
zipTests =
  TestLabel "zip'" $
    TestList
      [ mkTest "zips equal-length lists" ([(1,'a'), (2,'b'), (3,'c')] :: [(Int, Char)]) (zip' [1,2,3] ['a','b','c'])
      , mkTest "stops when first list empty" ([] :: [(Int, Char)]) (zip' [] ['a','b'])
      , mkTest "stops when second list empty" ([] :: [(Int, Char)]) (zip' [1,2] [])
      , mkTest "zips uneven lists to shorter length" ([(1,True), (2,False)] :: [(Int, Bool)]) (zip' [1,2,3] [True, False])
      , mkTest "comparison of zip function - stops with shorter right" (zip [1,2,3 :: Int] ['a','b']) (zip' [1,2,3 :: Int] ['a','b'])
      , mkTest "comparison of zip function - both empty" (zip ([] :: [Int]) ([] :: [Char])) (zip' ([] :: [Int]) ([] :: [Char]))
      ]

unzipTests :: Test
unzipTests =
  TestLabel "unzip'" $
    TestList
      [ mkTest "unzip empty list" (([], []) :: ([Int], [Char])) (unzip' [])
      , mkTest "unzip single pair" (([1], ['a']) :: ([Int], [Char])) (unzip' [(1, 'a')])
      , mkTest "unzip multiple pairs" (([1,2,3], ['a','b','c']) :: ([Int], [Char])) (unzip' [(1,'a'), (2,'b'), (3,'c')])
      , mkTest "comparison of unzip function - booleans" (unzip [(True,1),(False,2)]) (unzip' [(True,1),(False,2)])
      , mkTest "comparison of unzip function - duplicates" (unzip [(1,'x'),(1,'y')]) (unzip' [(1,'x'),(1,'y')])
      ]

deleteTests :: Test
deleteTests =
  TestLabel "delete'" $
    TestList
      [ mkTest "removes first occurrence" ([1,2,4] :: [Int]) (delete' 3 [1,2,3,4 :: Int])
      , mkTest "removes head element" ([1,2] :: [Int]) (delete' 1 [1,1,2 :: Int])
      , mkTest "leaves list unchanged when element missing" ([1,2,3,4] :: [Int]) (delete' 5 [1,2,3,4 :: Int])
      , mkTest "works on characters" "itimark" (delete' 'k' "kitimark")
      , mkTest "comparison of delete function - removes duplicate" (delete 2 [1,2,2,3 :: Int]) (delete' 2 [1,2,2,3 :: Int])
      , mkTest "comparison of delete function - missing character" (delete 'z' "kitimark") (delete' 'z' "kitimark")
      ]

intersectTests :: Test
intersectTests =
  TestLabel "intersect'" $
    TestList
      [ mkTest "intersect empty with list" ([] :: [Int]) (intersect' [] [1,2,3 :: Int])
      , mkTest "intersect list with empty" ([] :: [Int]) (intersect' [1,2,3 :: Int] [])
      , mkTest "intersect integers keeps order of first" ([3,4] :: [Int]) (intersect' [1,2,3,4 :: Int] [3,4,5])
      , mkTest "intersect duplicates" ([1,1,2,3] :: [Int]) (intersect' [1,1,2,3 :: Int] [1,2,2,3])
      , mkTest "intersect strings" "itimar" (intersect' "kitimark" "matrix")
      , mkTest "comparison of intersect function - disjoint lists" (intersect [1,2 :: Int] [3,4]) (intersect' [1,2 :: Int] [3,4])
      , mkTest "comparison of intersect function - repeated chars" (intersect "mississippi" "sim") (intersect' "mississippi" "sim")
      , mkTest "comparison of intersect function - intersect duplicates" (intersect [1,1,2,3 :: Int] [1,2,2,3]) (intersect' [1,1,2,3 :: Int] [1,2,2,3])
      , mkTest "comparison of intersect function - intersect strings" (intersect "kitimark" "matrix") (intersect' "kitimark" "matrix")
      ]

intersperseTests :: Test
intersperseTests =
  TestLabel "intersperse'" $
    TestList
      [ mkTest "empty list stays empty" ([] :: [Int]) (intersperse' 0 ([] :: [Int]))
      , mkTest "singleton list unaffected" ([42] :: [Int]) (intersperse' 0 [42 :: Int])
      , mkTest "inserts separators between numbers" ([1,0,2,0,3] :: [Int]) (intersperse' 0 [1,2,3 :: Int])
      , mkTest "works on strings" "k-i-t-i-m-a-r-k" (intersperse' '-' "kitimark")
      , mkTest "comparison of intersperse function - booleans" (intersperse False [True, True]) (intersperse' False [True, True])
      , mkTest "comparison of intersperse function - characters" (intersperse ':' "abc") (intersperse' ':' "abc")
      ]

intercalateTests :: Test
intercalateTests =
  TestLabel "intercalate'" $
    TestList
      [ mkTest "empty list stays empty" ([] :: [Int]) (intercalate' ([] :: [Int]) [])
      , mkTest "single sublist unaffected" ([1,2,3] :: [Int]) (intercalate' [0 :: Int] [[1,2,3]])
      , mkTest "joins sublists with separator" ([1,2,0,3,0,4,5] :: [Int]) (intercalate' [0] [[1,2],[3],[4,5 :: Int]])
      , mkTest "intercalates strings" "ki-ti-mark" (intercalate' "-" ["ki","ti","mark"])
      , mkTest "handles empty separators" "kitimark" (intercalate' "" ["kit","im","ark"])
      , mkTest "comparison of intercalate function - leading empties" (intercalate [9 :: Int] [[],[1,2]]) (intercalate' [9 :: Int] [[],[1,2]])
      , mkTest "comparison of intercalate function - text chunks" (intercalate "::" ["h","s","k"]) (intercalate' "::" ["h","s","k"])
      ]

permutationsTests :: Test
permutationsTests =
  TestLabel "permutations'" $
    TestList
      [ mkTest "empty list" ([[]] :: [[Int]]) (permutations' ([] :: [Int]))
      , mkTest "singleton list" [[2]] (permutations' [2])
      , mkTest "singleton list" (sort [[1, 2], [2, 1]]) (sort $ permutations' [1, 2])
      , mkTest "singleton list" (sort [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]) (sort $ permutations' [1..3])
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

compressTests :: Test
compressTests =
  TestLabel "compress" $
    TestList
      [ mkTest "compress empty string" "" (compress "")
      , mkTest "compress singleton char" "A1" (compress "A")
      , mkTest "compress homework example 1" "A7B4A4C4" (compress "AAAAAAABBBBAAAACCCC")
      , mkTest "compress homework example 2" "A25" (compress "AAAAAAAAAAAAAAAAAAAAAAAAA")
      , mkTest "compress homework example 3" "A1B1C1" (compress "ABC")
      ]

areAnagramTests :: Test
areAnagramTests =
  TestLabel "areAnagram" $
    TestList
      [ mkTest "areAnagram homework example 1" True (areAnagram "Tom Marvolo Riddle" "I am Lord Voldemort")
      , mkTest "areAnagram homework example 2" False (areAnagram "Tom Marvolo Riddle" "I am also Lord Voldemort")
      ]

caesarCipherTests :: Test
caesarCipherTests =
  TestLabel "caesarCipher" $
    TestList
      [ mkTest "caesarCipher homework example 1" "efghijklmnopqrstuvwxyzabcd" (caesarCipher 4 ['a'..'z'])
      , mkTest "caesarCipher homework example 2" "Jgbk Xgcozgz" (caesarCipher 6 "Dave Rawitat")
      , mkTest "caesarCipher homework example 3" "3215697" (caesarCipher 2 "3215697")
      , mkTest "caesarCipher homework example 4" "!!!!!@^&#%@@&*K#" (caesarCipher 2 "!!!!!@^&#%@@&*I#")
      ]

caesarDecipherTests :: Test
caesarDecipherTests =
  TestLabel "caesarDecipher" $
    TestList
      [ mkTest "caesarDecipher homework example 1" "abcdefghijklmnopqrstuvwxyz" (caesarDecipher 4 "efghijklmnopqrstuvwxyzabcd")
      , mkTest "caesarDecipher homework example 2" "Dave Rawitat" (caesarDecipher 6 "Jgbk Xgcozgz")
      , mkTest "caesarDecipher homework example 3" "Bytc Pyugryr" (caesarDecipher 8 "Jgbk Xgcozgz")
      ]

slowestSortTests :: Test
slowestSortTests =
  TestLabel "slowestSort" $
    TestList
      [ mkTest "sorts empty list" ([] :: [Int]) (slowestSort ([] :: [Int]))
      , mkTest "sorts singleton list" ([42] :: [Int]) (slowestSort [42 :: Int])
      , mkTest "sorts reverse list" ([1,2,3,4] :: [Int]) (slowestSort [4,3,2,1 :: Int])
      , mkTest "handles duplicates" ([1,1,2,3] :: [Int]) (slowestSort [3,1,2,1 :: Int])
      , mkTest "sorts characters" "aeklrt" (slowestSort "talker")
      , mkTest "comparison of sort function - integers" (sort [3,4,1,2 :: Int]) (slowestSort [3,4,1,2 :: Int])
      , mkTest "comparison of sort function - strings" (sort "kitimark") (slowestSort "kitimark")
      ]

allTests :: Test
allTests = TestList
  [ lengthTests
  , groupTests
  , nubTests
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
  , permutationsTests
  , insertionsortTests
  , insertTests
  , mergesortTests
  , mergeTests
  , stalinSortTests
  , compressTests
  , areAnagramTests
  , caesarCipherTests
  , caesarDecipherTests
  , slowestSortTests
  ]

main :: IO ()
main = do
  counts <- runTestTT allTests
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
