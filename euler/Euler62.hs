module Euler62 where

import Control.Monad (msum)
import Data.List (sort)
import Data.Map (Map, elems, filter, fromListWith)
import Prelude hiding (filter)
import Lib

cubes = take 10000 [x*x*x | x <- [1..]]
key = fromDigits . reverse . sort . digits
cubeMap = fromListWith (++) $ [(key c, [c]) | c <- cubes]
fiveCubeMap = filter (\cs -> length cs ==5) cubeMap
euler62 = print $ minimum $ concat $ elems $ fiveCubeMap
