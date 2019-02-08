module Euler61 where

import Control.Monad (msum)
import Data.List (permutations)
import Prelude
import Lib

figurates = [
  triangle,
  \n -> n * n,
  pentagon,
  hexagon,
  \n -> n * (5*n - 3) `div` 2,
  \n -> n * (3*n - 2)
  ]
range f = takeWhile (< 10000) $ dropWhile (< 1000) $ map f [1..]
ranges = map range $ reverse figurates

-- cd: first element of each result must begin with this
-- ab: last element of each result must end with this
search :: [[Int]] -> Int -> Int -> [[Int]]
search (ns:nss) cd ab = do
  n <- filter (\n -> n `div` 100 == cd) ns
  [n:rest | rest <- search nss (n `mod` 100) ab]
search [] cd ab = if cd == ab then [[]] else []

search2 :: [[Int]] -> Int -> [[Int]]
search2 nss n = do
  let (ab, cd) = n `divMod` 100
  nss' <- permutations nss
  [n:rest | rest <- search nss' cd ab]

search3 :: [[Int]] -> [[Int]]
search3 (ns:nss) = concat [search2 nss n | n <- ns]

euler61 =
  let solution = head $ search3 ranges
  in print $ sum solution
