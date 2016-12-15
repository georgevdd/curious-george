module Euler2 where

import Control.Monad.State as State
import Data.Bits (xor)
import Data.Char (isAlpha, chr, ord)
import Data.List (elemIndex, find, group, groupBy, inits, mapAccumL, maximumBy, minimumBy, minimum, nub,
                  partition, permutations, sort, sortBy, tails, transpose, unfoldr)
import Data.Maybe (catMaybes, fromJust, fromMaybe, listToMaybe)
import Data.Map as M hiding (foldl, foldr, map)
import Data.Set as S hiding (foldl, foldr, map)
import Data.Ord (comparing, Down(..))
import Data.Text as Text (filter, pack, split, unpack)
import GHC.Real

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

euler56 = maximum [(sum . digits) (a^b) | a <- [1..99], b <- [1..99]]

euler57 = length $ P.filter longerNumerator expansions
 where
  longerNumerator (a :% b) = length (digits a) > length (digits b)
  expansions = take 1000 [1 + 1 / r | r <- iterate expand (2 :: Ratio Integer)]
  expand x = 2 + 1 / x

euler58 = side
 where
  numbersOnSide s = [s*s - k*(s-1) | k <- [3,2,1]]  -- Ignore s*s because squares are always composite.
  accumulate (p, d, s) p' = (p+p', d+4, s+2)
  search = scanl accumulate (0, 1, 1) [count isPrime $ numbersOnSide r | r <- [3,5..]]
  ratios = [((fromIntegral p) / (fromIntegral d), s) | (p, d, s) <- search]
  Just (ratio, side) = find (\(r, s) -> r < 0.1) $ tail ratios

euler59 = do
  cipherText <- euler59data
  let key = guessKey cipherText
      plainText = decrypt key cipherText
  print [chr k | k <- key]
  print [chr c | c <- plainText]
  return $ sum plainText
 where
  keyLength = 3
  ctByKeyChar = transpose . chunks keyLength
  decrypt' keyChar ct = [c `xor` keyChar | c <- ct]
  decrypt key ct = concat $ transpose [
     decrypt' keyChar ctForChar
     | (keyChar, ctForChar) <- zip key (ctByKeyChar ct)]
  scoreKeyChar ctForChar keyChar =
    sum [fromMaybe 10000 $ elemIndex (ord c) plainTextChars | c <- "eta"]
   where plainTextChars = decrypt' keyChar $ sortByFrequency ctForChar
  guessKeyChar ctForChar = minimumBy (comparing $ scoreKeyChar ctForChar)
                                     [(ord 'a') .. (ord 'z')]
  guessKey ct = [guessKeyChar $ ctForChar | ctForChar <- ctByKeyChar ct]
  sortByFrequency xs = [head l | l <- sortBy (comparing $ Down . length)
                                             [l | l <- group $ sort xs]]

euler59data = do
  ordsText <- fmap Text.pack $ readFile "data/p059_cipher.txt"
  return ([read $ unpack s | s <- Text.split (==',') ordsText] :: [Int])
