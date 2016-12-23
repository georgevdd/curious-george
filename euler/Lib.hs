module Lib where

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

isqrt = round . sqrt . fromIntegral

(.:) = (.) . (.)
equalBy f x y = (f x) == (f y)

count = length .: P.filter

chunks = unfoldr . nextChunk
 where
  nextChunk _ [] = Nothing
  nextChunk n l = (Just .: splitAt) n l

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM pred (x:xs) = do
  p <- pred x
  if p
  then do xs' <- takeWhileM pred xs
          return (x:xs')
  else return []
takeWhileM _ [] = return []

-- Test membership of a possibly-infinite ascending list
x `elemAsc` xs = (==x) . head $ dropWhile (<x) xs

-- ### Combinatorics ### --

factorial n = product [1..n]
n `ncr` r = factorial n `div` (factorial r * factorial (n - r))

-- Given a sorted list, produces its permutations in lexicographical order
lexPerms [] = [[]]
lexPerms l = concat [[x:l'' | l'' <- lexPerms l'] | (x, l') <- subs l]
  where
    subs l  = [(head t, i ++ tail t) |
               (i, t) <- init $ zip (Data.List.inits l) (Data.List.tails l)]

choices' :: [a] -> Int -> [([a], [a])]
choices' []     0 = [([], [])]
choices' []     _ = []
choices' (x:xs) r = [(x:ys, zs) | (ys, zs) <- choices' xs (r-1)] ++
                    [(ys, x:zs) | (ys, zs) <- choices' xs  r   ]

choices = map fst .: choices'

partitions [] = [([], [])]
partitions (x:xs) = concat [[(x:ys, zs), (ys, x:zs)] | (ys, zs) <- partitions xs]


-- ### Number theory and algebra ### --

x `divides` y = y `mod` x == 0
num_divisors = product . map ((+1) . length) . group . prime_factors
divisors n = [x | x <- [1..n `div` 2], x `divides` n]
perfect n = sum (divisors n) == n
abundant n = sum (divisors n) > n
amicable x = let y = d(x) in y /= x && d(y) == x
  where d(x) = sum $ divisors x

quadratic_roots a b c = [((-b) `op` sqrt(b*b - 4.0*a*c)) / (2.0*a) |
                         op <- [(+), (-)]]

-- Is x a whole number?
integral :: RealFrac a => a -> Bool
integral x = x == fromIntegral (truncate x)

triangle n = n * (n+1) `div` 2
-- x = n(n+1)/2 so n = (-0.5 +- sqrt(0.25 + 2x)) by the quadratic formula
triangular :: Integral a => a -> Bool
triangular = integral . troot . fromIntegral
 where
  troot x = head $ quadratic_roots 0.5 0.5 (-x)

pentagon n = n * (3*n - 1) `div` 2
pentagonal :: Integral a => a -> Bool
pentagonal = integral . proot . fromIntegral
 where
  proot x = head $ quadratic_roots 1.5 (-0.5) (-x)

hexagon n  = n * (2*n - 1)

pythagorean (a, b, c) = a*a + b*b == c*c

powmod _ a 1 = a
powmod m a b | (b `mod` 2) == 0 = powmod m (a*a `mod` m) (b `div` 2)
             | otherwise = a * powmod m a (b-1) `mod` m


-- ### Prime numbers ### --

-- O(Nsqrt(N))
primes = 2:3:[x | x <- [5..], isPrime x]
isPrime x = not $ any (`divides` x) (takeWhile (<= isqrt x) primes)

prime_factors 1 = []
prime_factors n = let m = head [x | x <- primes, x `divides` n] in m : prime_factors (n `div` m)

type Sieve = (S.Set Int, [Int])
emptySieve' :: [Int] -> Sieve
emptySieve' xs = (S.empty, xs)

emptySieve = emptySieve' primes

prepareSieveUpTo :: Int -> Sieve
prepareSieveUpTo n = execState (takeWhileM (fmap not . testPrime) [n..]) emptySieve

testSieve :: Int -> State Sieve Bool
testSieve x = do
  (cache, ps) <- get
  when (x >= head ps)
       (modify (fillCacheTo x))
  cc <- gets fst
  return $ x `S.member` cc
 where
  fillCacheTo x (cache, ps) =
    let (more, ps') = break (>x) ps
    in (cache `S.union` (S.fromDistinctAscList more), ps')

testPrime = testSieve

-- For making a huge prime sieve, with a crude progress counter
sieveUpTo :: Int -> IO Sieve
sieveUpTo 0 = return emptySieve
sieveUpTo n = do
  s' <- sieveUpTo (n - k)
  seq (head $ snd s') (putChar '.')
  return $ execState (mapM testPrime [max 0 (n-k+1)..n]) s'
 where k = 10^6


-- ### Other integer sequences ### --

fibs = 1 : 1 : map (uncurry (+)) (zip fibs (tail fibs))
triangulars :: Integral a => [a]
triangulars = map triangle [1..]
hexagonals = map hexagon [1..]

-- Numbers that in base `b` are palindromic and have `n` digits (including
-- those with leading zeros)
palindromes b 1 = [0..b-1]
palindromes b 2 = [(b+1) * d | d <- [0..b-1]]
palindromes b n = let ps = palindromes b (n-2)
                      x = b^(n-1) + 1
                      xs = [x * d | d <- [0..b-1]]
                  in [p*b + x | x <- xs, p <- ps]

-- Test for palindrome-ness in base 10
palindromic n = n == (fromDigits . reverse . digits) n


--prime_decomposition = map (head &&& length) . group . prime_factors



-- enumerates all pairs of positive integers, smallest sum first.
diagonals :: Integral a => [(a, a)]
diagonals = concat [[(x, n-x) | x <- [1..n-1]] | n <- [1..]]

-- enumates pairs of positive integers where the first is no larger than the
-- second, smallest sum first.
diagonals' :: Integral a => [(a, a)]
diagonals' = concat [[(x, n-x) | x <- [1..n `div` 2]] | n <- [1..]]


-- ### Collatz conjecture ### --

collatz_step n | 2 `divides` n = n `div` 2
               | otherwise = 3*n + 1
collatz = takeWhile (/=1) . iterate collatz_step

collatz' m n = case M.lookup n m of
                 Just k -> (m, k)
                 Nothing ->
                     let n' = collatz_step n
                         (m', k') = collatz' m n'
                     in (M.insert n (k'+1) m', k'+1)


-- ### Number triangles (e.g. Problem 18) ### --

triMax :: [[Int]] -> Int
triMax = head . foldr1 step
  where step line1 line2 = [max x y | (x, y) <- zip [a + b  | (a, b ) <- zip line1 line2       ]
                                                    [a + b' | (a, b') <- zip line1 (tail line2)]]


-- ### Written representations ### --

digits :: (Integral a, Show a, Read a) => a -> [a]
digits = map ((read :: (Integral a, Read a) => String -> a) . return) . show

fromDigits :: (Integral a) => [a] -> a
fromDigits = foldl (\n d -> 10 * n + d) 0

x `inbase` b = reverse $ unfoldr (\x -> guard (x/=0) >> Just (x `mod` b, x `div` b)) x

unitsNames = [
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine"
  ]

teensNames = [
  "eleven",
  "twelve",
  "thirteen",
  "fourteen",
  "fifteen",
  "sixteen",
  "seventeen",
  "eighteen",
  "nineteen"
  ]

tensNames = [
  "ten",
  "twenty",
  "thirty",
  "forty",
  "fifty",
  "sixty",
  "seventy",
  "eighty",
  "ninety"
  ]

spellNumber n =
  let
    (h, t, u) = (n `div` 100, n `div` 10 `mod` 10, n `mod` 10)
    spell 0 0 u = unitsNames !! (u - 1)
    spell 0 t 0 = tensNames !! (t - 1)
    spell 0 t u = if t == 1
                  then teensNames !! (u - 1)
                  else spell 0 t 0 ++ "-" ++ spell 0 0 u
    spell h 0 0 = unitsNames !! (h - 1) ++ " hundred"
    spell h t u = spell h 0 0 ++ " and " ++ spell 0 t u
  in if n == 1000 then "one thousand" else spell h t u


-- ### Date and time ### --

monthDays year = [
  31, febDays year, 31,
  30, 31, 30,
  31, 31, 30,
  31, 30, 31]
febDays year | year `mod` 400 == 0 = 29
             | year `mod` 100 == 0 = 28
             | year `mod` 4 == 0 = 29
             | otherwise = 28


-- ### Currency ### --
changes :: Int -> Int -> [[Int]]
changes m p = do
  coin <- denoms
  if coin > p || coin > m then []
  else if coin == p then [[coin]]
       else [(coin:rest) | rest <- changes coin (p-coin)]
 where
  denoms = reverse [1, 2, 5, 10, 20, 50, 100, 200]
