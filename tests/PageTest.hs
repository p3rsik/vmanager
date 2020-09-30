{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Foundation

import Page
import Types

qsort :: [Int] -> [Int]
qsort []     = []
qsort (x:xs) = qsort lhs <> [x] <> qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs


-- page table with random values for testing
pTable :: PageTable
pTable = ([(Page (Fid fid) (Age 1) False False (Pid pid)) | fid <- [1..128],
                                             pid <- [1..128]],
          [(Page (Fid fid) (Age 1) False False (Pid pid)) | fid <- [1..128],
                                             pid <- [1..128]])

idToOffTest :: Property
idToOffTest = not (null ([])) ==>
  not $ null $ filter ( \ f -> (==) f $ offToId . idToOff $ f)
                                [(Fid fid) | fid <- [1..128]]

offToIdTest :: Property
offToIdTest = not (null ([])) ==>
  not $ null $ filter (\f -> (==) f $ idToOff . offToId $ f)
                                [(Offset fid) | fid <- [1..128]]

-- prop_maximum ::  [Int] -> Property
-- prop_maximum xs = not (null xs) ==>
--                  last (qsort xs) == maximum xs

main :: IO Bool
main = $(quickCheckAll)
