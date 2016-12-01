module Euler2 where

import Control.Monad.State as State
import Data.Char (isAlpha, ord)
import Data.List (elemIndex, find, group, groupBy, inits, mapAccumL, maximumBy, minimum, nub,
                  partition, permutations, sort, sortBy, tails, unfoldr)
import Data.Maybe (catMaybes, fromJust, fromMaybe, listToMaybe)
import Data.Map as M hiding (foldl, foldr, map)
import Data.Set as S hiding (foldl, foldr, map)
import Data.Ord (comparing)
import Data.Text as Text (filter, pack, split, unpack)

import Debug.Trace

import Prelude as P

import Cards as Cards
import Lib

euler53 = length $ P.filter (> 1000000) [n `ncr` r | n <- [1..100], r <- [1..n]]

euler54 = do
  handData <- readFile "data/p054_poker.txt"
  let cards = [splitAt 5 [read h | h <- words l] | l <- lines handData]
  return $ length (P.filter player1Wins cards)
 where
  player1Wins (h1, h2) = pokerScore h1 > pokerScore h2

euler55 = length $ P.filter lychrel [1..10000-1]
 where
  lychrel n = not $ any palindromic $ take 50 $ iterate addRev (addRev n)
  addRev n = n + (fromDigits . reverse . digits) n
