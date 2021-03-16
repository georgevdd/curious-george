module Euler65 where

import Data.Ratio (numerator)
import Lib (digits)

-- Expand a continued fraction, given
-- its defining sequence
expandCf = foldr1 (\r n -> r + 1/n) . map toRational

eCf = 2 : concat [[1,k,1] | k <- [2,4..]]

answer = sum $ digits $ numerator $ expandCf $ take 100 eCf
euler65 = print answer
