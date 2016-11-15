module Euler where

import Control.Monad.State as State
import Data.Char (isAlpha, ord)
import Data.List (elemIndex, find, group, groupBy, inits, mapAccumL, maximumBy, minimum,
                  partition, permutations, sort, sortBy, tails, unfoldr)
import Data.Maybe (fromJust, fromMaybe)
import Data.Map as M hiding (foldl, foldr, map)
import Data.Set as S hiding (foldl, foldr, map)
import Data.Ord (comparing)
import Data.Text as Text (filter, pack, split, unpack)

import Prelude as P

isqrt = round . sqrt . fromIntegral

(.:) = (.) . (.)
equalBy f x y = (f x) == (f y)

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM pred (x:xs) = do
  p <- pred x
  if p
  then do xs' <- takeWhileM pred xs
          return (x:xs')
  else return []
takeWhileM _ [] = return []


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


-- ### Prime numbers ### --

-- O(Nsqrt(N))
primes = 2:3:[x | x <- [5..], not $ any (`divides` x) (takeWhile (<= isqrt x) primes)]

prime_factors 1 = []
prime_factors n = let m = head [x | x <- primes, x `divides` n] in m : prime_factors (n `div` m)

type Sieve = (S.Set Int, [Int])
emptySieve :: Sieve
emptySieve = (S.empty, primes)

testPrime :: Int -> State Sieve Bool
testPrime x = do
  (cache, ps) <- get
  when (x >= head ps)
       (modify (fillCacheTo x))
  cc <- gets fst
  return $ x `S.member` cc
 where
  fillCacheTo x (cache, ps) =
    let (more, ps') = break (>x) ps
    in (cache `S.union` (S.fromDistinctAscList more), ps')

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


--prime_decomposition = map (head &&& length) . group . prime_factors



-- enumerates all pairs of positive integers, smallest sum first.
diagonals = concat [[(x, n-x) | x <- [1..n-1]] | n <- [1..]]

-- enumates pairs of positive integers where the first is no larger than the
-- second, smallest sum first.
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

triMax = head . foldr1 step
  where step line1 line2 = [max x y | (x, y) <- zip [a + b  | (a, b ) <- zip line1 line2       ]
                                                    [a + b' | (a, b') <- zip line1 (tail line2)]]


-- ### Written representations ### --

digits = map ((read :: (Integral a, Read a) => String -> a) . return) . show
fromDigits = foldl (\n d -> 10 * n + d) 0

x `inbase` b = reverse $ unfoldr (\x -> guard (x/=0) >> Just (x `mod` b, x `div` b)) x

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


-- ### Problems ### --

euler1 = sum [x | x <- [1..999], 3 `divides` x || 5 `divides` x]
euler2 = sum [x | x <- takeWhile (< 4000000) fibs, 2 `divides` x]
euler3 = maximum $ prime_factors 600851475143
-- Brute forceful but good enough
euler4 = maximum [x*y | x <- [999,998..900], y <- [999,998..900], (x*y) `elem` (take 100 palindromes)]
 where
  palindromes = [100001 * a + 10010 * b + 1100 * c | a <- ns, b <- ns, c <- ns]
  ns = reverse [0..9]

euler5 = foldr1 lcm [1..20]
euler6 = square (sum [1..100]) - sum (map square [1..100]) where square x = x*x
euler7 = primes!!10000
euler8 = maximum $ map (product . map (read . return) . take 5) $ tails euler8data
euler9 = multiply . fromJust . find pythagorean . map triple $ diagonals'
  where triple (a, b) = (a, b, 1000 - (a+b))
        multiply (a, b, c) = a*b*c
euler10 = sum $ takeWhile (< 2000000) primes
euler11 = maximum [product q | q <- qs]
  where qs = concat [hqs, vqs, dqs]
        hqs = concatMap scan rows
        vqs = [map (!!n) w | n <- [0..19], w <- scan rows]
        dqs = [map (\(r,x)->r!!(n+x)) (zip w ds) |
               n <- [0..16],
               w <- scan rows,
               ds <- [[0..3], [3,2..0]]]
        scan = map (take 4) . take 17 . tails
        rows = euler11data
euler12 = fromJust $ find ((>500) . num_divisors) triangulars
euler13 = take 10 $ show $ sum euler13data
euler14 = fst . maximumBy (comparing snd) . zip [1..] . snd $ mapAccumL collatz' (M.singleton 1 1) [1..999999]
euler15 = 40 `ncr` 20
euler16 = sum $ digits (2 ^ 1000)
euler17 = sum $ [length [c | c <- spellNumber n, isAlpha c] | n <- [1..1000]]
euler18 = triMax euler18data
euler19 = length [x | x <- scanl (+) (1 + 365) (concat [monthDays y | y <- [1901..2000]]), 7 `divides` x]
euler20 = sum $ digits $ factorial 100
euler21 = sum [x | x <- [1..9999], amicable x]
euler22 = do
  names <- euler22data
  let score name = sum [ord c - ord '@' | c <- name]
  return $ sum [i * score n | (i, n) <- zip [1..] $ sort names]
-- Requires optimization
euler23 = sum [x | x <- [1..28123], not $ expressible x]
  where
    abundants = S.fromDistinctAscList [n | n <- [1..28123], abundant n]
    expressible n =
      let xs = takeWhile (< n) $ S.toAscList abundants
          hasY x = S.member (n - x) abundants
      in any hasY xs
-- Requires optimization
euler24 = read $ concat [show d | d <- (lexPerms [0..9])!!(1000000-1)] :: Integer
euler25 = length (takeWhile (<10^(1000-1)) fibs) + 1

euler26 =
 let nextDigit n d = if n == 0 then Nothing else Just (quotRem n d)
     longDiv n d = unfoldr (liftM (\x -> (x, 10 * snd x)) . (flip nextDigit $ d)) n
     cycleLength d = fmap (+1) . msum . snd $ mapAccumL (\l -> \e -> (e:l, elemIndex e l)) [] (longDiv 1 d)
 in maximumBy (comparing $ fromMaybe 0 . cycleLength) [1..1000]

-- euler27
-- b must be prime otherwise n=0 is composite
-- a+b must be even otherwise n=1 == 1+a+b is composite
euler27 = let (a, b, _) = bestTriple in a*b
 where
  quadratic a b n = n*n + a*n + b
  coeffs = do
    b <- concat [[x, -x] | x <- takeWhile (<= 1000) primes]
    a <- concat [[x, -x] | x <- if b `mod` 2 == 0
                                then [0, 2 .. 1000]
                                else [1, 3 .. 1000]]
    return (a, b)
  primeRun a b = fmap length $ takeWhileM (testPrime . quadratic a b) [0..]
  runLength a b = do l <- primeRun a b
                     return (a, b, l)
  runLengths = mapM (uncurry runLength) coeffs
  triples = evalState runLengths emptySieve
  bestTriple = maximumBy (comparing (\(a, b, k) -> k)) triples

euler28 = 1 + 4 * sum [4*k*k + k + 1 | k <- [1..500]]
euler29 = S.size $ foldr1 S.union [S.fromList [a^b | b <- [2..100]] | a <- [2..100]]
euler30 = sum [x | x <- [1..100000], sum [d^4 | d <- digits x] == x] - 1

euler31 = length $ changes 200 200

-- If x has p digits and y has q digits then x*y has either p+q-1 digits or
-- p+q digits. If x*y == z where z has r digits, then r == p+q-1 or r == p+q.
-- If p+q+r == 9 then p+q+p+q-1 == 9 or p+q+p+q == 9. The second of these is
-- not possible so p+q == 5.
euler32 = P.sum $ S.toList $ S.fromList pandigitals
 where
  pandigitals = do
   (xy_digits, z_digits) <- choices' [1..9] 5
   (x_digits, y_digits) <- P.filter neither_null $ partitions xy_digits
   x <- fmap fromDigits $ permutations x_digits
   y <- fmap fromDigits $ permutations y_digits
   z <- fmap fromDigits $ permutations z_digits
   if x * y == z then [z] else []
  neither_null (a, b) = not (P.null a || P.null b)

euler33 = b' `div` gcd a' b'
 where
  (a', b') = (product as, product bs)
  (as, bs) = unzip curiosities
  curiosities = [(a, b) | a <- [10..99], b <- [1..99], curious (a,b)]
  curious (a,b) = (a `mod` 10) == (b `div` 10) &&
                  a * (b `mod` 10) == b * (a `div` 10) &&
                  a `mod` 11 /= 0

-- 8 * factorial 9 only gives a 7-digit number so no >7-digit number is equal
-- to the sum of the factorials of its digits
euler34 = sum [x | x <- [3..9999999], x == sdf x]
 where
  sdf x = sum [fs !! d | d <- rdigits x]
  fs = map factorial [0..9]
  rdigits = unfoldr (\n -> if n == 0
                           then Nothing
                           else Just (n `mod` 10, n `div` 10))

euler35 = length $ P.filter circular ps
 where
  ps = takeWhile (<1000000) primes
  ps' = S.fromList ps
  circular = all (`S.member` ps') . rotations'
rotations' = map fromDigits . rotations . digits
rotations xs = [rotate xs n | n <- [1..(length xs)-1]]
rotate xs n = let (a, b) = splitAt n xs in b++a

euler36 = sum . S.toList $ palindromes' 10 6 `S.intersection` palindromes' 2 20
 where
  palindromes' b n = S.fromList [x | x <- concat [palindromes b n' | n' <- [1..n]], x `mod` b /= 0]

euler37 = sum $ take 11 $ evalState (filterM truncatable primes') emptySieve
 where
  primes' = dropWhile ((<2) . length . digits) primes
  truncatable n = fmap P.null $ filterM (fmap not . testPrime)
                                        (lTruncs n ++ rTruncs n)
  truncs subs n = [fromDigits ds | ds <- subs $ digits n, not $ P.null ds]
  lTruncs = truncs tails
  rTruncs = truncs inits

euler38 = maximum . map fromDigits . concat $ [candidate n | n <- [2..5]]
 where
  candidate n = take 1 $ P.filter pandigital [concatProd x n | x <- dDigitNumbers $ 9 `div` n]
  dDigitNumbers d = [10^d-1, 10^d-2 .. 10^(d-1)]
  pandigital = (== [1..9]) . sort
  concatProd x n = concat [digits (x*n') | n' <- [1..n]]

euler39 = (fst . head . head .
           sortBy (comparing $ negate . length) .
           groupBy (equalBy fst) .
           sortBy (comparing fst)
          ) [(sum3 t, t) | t <- triples]
 where
  triples = P.filter pythagorean . takeWhile ((<1000) . sum3) $ map triple diagonals'
  triple (a, b) = (a, b, isqrt (a*a + b*b))
  sum3 (a, b, c) = a+b+c

euler40 = product [champernowneDigit $ 10^x-1 | x <- [0..6]]
 where
  -- Compute d'th digit of Champernowne's number
  champernowneDigit d =
    let -- Segment and index of digit within it
        (seg, segDigit) = segRem d
        -- Index of integer within segment and index of digit within integer
        (n', nDigit) = quotRem segDigit (seg+1)
        -- Actual integer
        n = 10^seg + n'
    in -- Actual digit within integer
       n `div` 10^(seg-nDigit) `mod` 10
  -- Segment n contains all the positive integers with n digits
  -- Segment 0 is [1..9], segment 1 is [10..99], etc
  -- [1..9] is 9 numbers, [10..99] is 90 numbers, [100..999] is 900 numbers, etc
  segNums s = 9 * 10^s
  -- [1..9] is 9 digits, [10..99] is 180 digits, [100..999] is 2700 digits, etc
  segDigits s = (s+1) * segNums s
  -- Compute segment and digit within segment.
  segRem d = last . takeWhile ((>=0) . snd) . zip [0..] $ scanl (-) d $ map segDigits [0..]

euler41 = head $ evalState (filterM testPrime ps) emptySieve
 where
  -- Don't need to generate 8- and 9- digit numbers because they
  -- will always be multiples of 3
  ps = map fromDigits . reverse $ lexPerms [1..7]

euler42 = do
  ws <- euler42data
  return $ length . P.filter (triangular . wordNum) $ ws
 where
  letterNum = (+ (-64)) . ord
  wordNum = sum . map letterNum
  triangular n = (==n) . head $ dropWhile (<n) triangulars

euler43 = sum [fromDigits $ reverse ds |
               ds <- foldM addDigits [] ([1,1,1] ++ take 7 primes)]
 where
  addDigits ds p = [d:ds | d <- nextDigits ds p]
  nextDigits l 1 = [d | d <- [0..9], not $ d `elem` l]
  nextDigits l@(d1:d2:ds) p = [d | d <- [0..9],
                               p `divides` (d2*100+d1*10+d),
                               not $ d `elem` l]

euler45 = head $ tail $ P.filter (\n -> triangular n && pentagonal n) $ drop 142 hexagonals


euler67_broken = do
  triText <- readFile "euler/p067_triangle.txt"
  let tri = [[(read n) :: Integer | n <- words l] | l <- lines triText]
  return $ triMax tri


{-- This is all broken euler67 stuff
-- Input is lower and upper bounds, inclusive
-- Result is (worst cost, max comparisons)
worstCost :: Integer
          -> Integer
          -> State (Map Integer (Integer, Integer, [Integer]))
                   (Integer, Integer, [Integer])
worstCost 0 0 = return (0, 0, [])
worstCost 0 y = do
  m <- get
  case M.lookup y m of
       Just r -> return r
       Nothing -> do
           cs <- mapM (recCost 0 y) [0..y]
           m' <- get
           let r = minimum cs
           --put $ M.insert y r m'
           return r
worstCost x y = do
  (c, n, l) <- worstCost 0 (y-x)
  return (c + x*n, n, [k+x | k <- l])
-- Worst cost of searching (x, y) if the first probe is at k
recCost x y k = do
  lower <- if x < k then (worstCost x (k-1)) else return (0, 0, [])
  upper <- if y > k then (worstCost (k+1) y) else return (0, 0, [])
  let (c, n, l) = max lower upper
  return (k + c, 1 + n, k:l)

f y = mapM (worstCost 1) [1..y]

g y = evalState (f y) M.empty
--}


euler8data = "73167176531330624919225119674426574742355349194934" ++
             "96983520312774506326239578318016984801869478851843" ++
             "85861560789112949495459501737958331952853208805511" ++
             "12540698747158523863050715693290963295227443043557" ++
             "66896648950445244523161731856403098711121722383113" ++
             "62229893423380308135336276614282806444486645238749" ++
             "30358907296290491560440772390713810515859307960866" ++
             "70172427121883998797908792274921901699720888093776" ++
             "65727333001053367881220235421809751254540594752243" ++
             "52584907711670556013604839586446706324415722155397" ++
             "53697817977846174064955149290862569321978468622482" ++
             "83972241375657056057490261407972968652414535100474" ++
             "82166370484403199890008895243450658541227588666881" ++
             "16427171479924442928230863465674813919123162824586" ++
             "17866458359124566529476545682848912883142607690042" ++
             "24219022671055626321111109370544217506941658960408" ++
             "07198403850962455444362981230987879927244284909188" ++
             "84580156166097919133875499200524063689912560717606" ++
             "05886116467109405077541002256983155200055935729725" ++
             "71636269561882670428252483600823257530420752963450"

euler11data =  [[08,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91,08],
                [49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00],
                [81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65],
                [52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91],
                [22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80],
                [24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50],
                [32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70],
                [67,26,20,68,02,62,12,20,95,63,94,39,63,08,40,91,66,49,94,21],
                [24,55,58,05,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72],
                [21,36,23,09,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95],
                [78,17,53,28,22,75,31,67,15,94,03,80,04,62,16,14,09,53,56,92],
                [16,39,05,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57],
                [86,56,00,48,35,71,89,07,05,44,44,37,44,60,21,58,51,54,17,58],
                [19,80,81,68,05,94,47,69,28,73,92,13,86,52,17,77,04,89,55,40],
                [04,52,08,83,97,35,99,16,07,97,57,32,16,26,26,79,33,27,98,66],
                [88,36,68,87,57,62,20,72,03,46,33,67,46,55,12,32,63,93,53,69],
                [04,42,16,73,38,25,39,11,24,94,72,18,08,46,29,32,40,62,76,36],
                [20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,04,36,16],
                [20,73,35,29,78,31,90,01,74,31,49,71,48,86,81,16,23,57,05,54],
                [01,70,54,71,83,51,54,69,16,92,33,48,61,43,52,01,89,19,67,48]]

euler13data = [
         37107287533902102798797998220837590246510135740250,
         46376937677490009712648124896970078050417018260538,
         74324986199524741059474233309513058123726617309629,
         91942213363574161572522430563301811072406154908250,
         23067588207539346171171980310421047513778063246676,
         89261670696623633820136378418383684178734361726757,
         28112879812849979408065481931592621691275889832738,
         44274228917432520321923589422876796487670272189318,
         47451445736001306439091167216856844588711603153276,
         70386486105843025439939619828917593665686757934951,
         62176457141856560629502157223196586755079324193331,
         64906352462741904929101432445813822663347944758178,
         92575867718337217661963751590579239728245598838407,
         58203565325359399008402633568948830189458628227828,
         80181199384826282014278194139940567587151170094390,
         35398664372827112653829987240784473053190104293586,
         86515506006295864861532075273371959191420517255829,
         71693888707715466499115593487603532921714970056938,
         54370070576826684624621495650076471787294438377604,
         53282654108756828443191190634694037855217779295145,
         36123272525000296071075082563815656710885258350721,
         45876576172410976447339110607218265236877223636045,
         17423706905851860660448207621209813287860733969412,
         81142660418086830619328460811191061556940512689692,
         51934325451728388641918047049293215058642563049483,
         62467221648435076201727918039944693004732956340691,
         15732444386908125794514089057706229429197107928209,
         55037687525678773091862540744969844508330393682126,
         18336384825330154686196124348767681297534375946515,
         80386287592878490201521685554828717201219257766954,
         78182833757993103614740356856449095527097864797581,
         16726320100436897842553539920931837441497806860984,
         48403098129077791799088218795327364475675590848030,
         87086987551392711854517078544161852424320693150332,
         59959406895756536782107074926966537676326235447210,
         69793950679652694742597709739166693763042633987085,
         41052684708299085211399427365734116182760315001271,
         65378607361501080857009149939512557028198746004375,
         35829035317434717326932123578154982629742552737307,
         94953759765105305946966067683156574377167401875275,
         88902802571733229619176668713819931811048770190271,
         25267680276078003013678680992525463401061632866526,
         36270218540497705585629946580636237993140746255962,
         24074486908231174977792365466257246923322810917141,
         91430288197103288597806669760892938638285025333403,
         34413065578016127815921815005561868836468420090470,
         23053081172816430487623791969842487255036638784583,
         11487696932154902810424020138335124462181441773470,
         63783299490636259666498587618221225225512486764533,
         67720186971698544312419572409913959008952310058822,
         95548255300263520781532296796249481641953868218774,
         76085327132285723110424803456124867697064507995236,
         37774242535411291684276865538926205024910326572967,
         23701913275725675285653248258265463092207058596522,
         29798860272258331913126375147341994889534765745501,
         18495701454879288984856827726077713721403798879715,
         38298203783031473527721580348144513491373226651381,
         34829543829199918180278916522431027392251122869539,
         40957953066405232632538044100059654939159879593635,
         29746152185502371307642255121183693803580388584903,
         41698116222072977186158236678424689157993532961922,
         62467957194401269043877107275048102390895523597457,
         23189706772547915061505504953922979530901129967519,
         86188088225875314529584099251203829009407770775672,
         11306739708304724483816533873502340845647058077308,
         82959174767140363198008187129011875491310547126581,
         97623331044818386269515456334926366572897563400500,
         42846280183517070527831839425882145521227251250327,
         55121603546981200581762165212827652751691296897789,
         32238195734329339946437501907836945765883352399886,
         75506164965184775180738168837861091527357929701337,
         62177842752192623401942399639168044983993173312731,
         32924185707147349566916674687634660915035914677504,
         99518671430235219628894890102423325116913619626622,
         73267460800591547471830798392868535206946944540724,
         76841822524674417161514036427982273348055556214818,
         97142617910342598647204516893989422179826088076852,
         87783646182799346313767754307809363333018982642090,
         10848802521674670883215120185883543223812876952786,
         71329612474782464538636993009049310363619763878039,
         62184073572399794223406235393808339651327408011116,
         66627891981488087797941876876144230030984490851411,
         60661826293682836764744779239180335110989069790714,
         85786944089552990653640447425576083659976645795096,
         66024396409905389607120198219976047599490197230297,
         64913982680032973156037120041377903785566085089252,
         16730939319872750275468906903707539413042652315011,
         94809377245048795150954100921645863754710598436791,
         78639167021187492431995700641917969777599028300699,
         15368713711936614952811305876380278410754449733078,
         40789923115535562561142322423255033685442488917353,
         44889911501440648020369068063960672322193204149535,
         41503128880339536053299340368006977710650566631954,
         81234880673210146739058568557934581403627822703280,
         82616570773948327592232845941706525094512325230608,
         22918802058777319719839450180888072429661980811197,
         77158542502016545090413245809786882778948721859617,
         72107838435069186155435662884062257473692284509516,
         20849603980134001723930671666823555245252804609722,
         53503534226472524250874054075591789781264330331690]


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

euler18data = [
  [75],
  [95, 64],
  [17, 47, 82],
             [18, 35, 87, 10],
  [20, 04, 82, 47, 65],
  [19, 01, 23, 75, 03, 34],
  [88, 02, 77, 73, 07, 63, 67],
  [99, 65, 04, 28, 06, 16, 70, 92],
  [41, 41, 26, 56, 83, 40, 80, 70, 33],
  [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
  [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
  [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
  [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
  [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
  [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

euler22data = do
  namesText <- fmap Text.pack $ readFile "euler/p022_names.txt"
  let namesQuoted = Text.split (==',') namesText
  return [unpack $ Text.filter (/='\"') n | n <- namesQuoted]

euler42data = do
  wordsText <- fmap Text.pack $ readFile "euler/p042_words.txt"
  let wordsQuoted = Text.split (==',') wordsText
  return [unpack $ Text.filter (/='\"') n | n <- wordsQuoted]
