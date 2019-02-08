module Euler63 where

powers n = takeWhile (< 10^n) $ dropWhile (< 10^(n-1)) [x^n | x <- [1..9]]
answer = length $ concat [powers n | n <- [1..1000]]
euler63 = print answer
