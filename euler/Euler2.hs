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

import Lib

euler55 = length $ P.filter lychrel [1..10000-1]
 where
  lychrel n = not $ any palindromic $ take 50 $ iterate addRev (addRev n)
  addRev n = n + (fromDigits . reverse . digits) n
