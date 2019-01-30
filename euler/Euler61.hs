module Euler61 where

import Control.Monad (msum)
import Data.List (permutations)
import Prelude as P
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
search :: [[Integer]] -> Integer -> Integer -> [[Integer]]
search (ns:nss) cd ab = do
  n <- P.filter (\n -> n `div` 100 == cd) ns
  nss' <- permutations nss
  rest <- search nss' (n `mod` 100) ab
  return $ n:rest
search [] cd ab = if cd == ab then [[]] else []

search2 :: [[Integer]] -> Integer -> [[Integer]]
search2 nss n =
  let (ab, cd) = n `divMod` 100
  in [n:rest | rest <- search nss cd ab]

search3 :: [[Integer]] -> [[Integer]]
search3 nss = do
  nss' <- permutations nss
  n <- head nss'
  search2 (tail nss') n

euler61 =
  let solution = head $ search3 ranges
  in sum solution
