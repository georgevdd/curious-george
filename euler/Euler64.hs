module Euler64 where

-- a = x. y = b/(sqrt(N) - c) = b(sqrt(N) + c)/(N - c^2) = (sqrt(N) + c)/((N - c^2)/b).
-- a' = floor(y). b' = ((N - c^2)/b). c' = c - a'*(N-c^2)/b
step n (a, b, c) = (a', b', c')
 where
  y = (fromIntegral b) / (sqrt (fromIntegral n) - fromIntegral c)
  a' = floor(y)
  b' = ((n - c*c) `div` b)
  c' = a' * b' - c

steps n = tail $ iterate (step n) (0, n, 0)

cf n =
  if a0*a0 == n
  then (a0, [])
  else (a0, a1:as)
 where
  (a0, _, _) : s@(a1, _, _) : ss = steps n
  as = [a | (a, _, _) <- takeWhile (/=s) ss]

oddPeriod (a0, as) = length as `mod` 2 == 1

answer = length [n | n <- [1..10000], oddPeriod $ cf n]
euler64 = print answer
