module Clip where
import Data.Maybe
import Data.Either
import Data.Vect.Double

import KiteDart

type HalfSpace = (Vec3, Scalar)
type Bounds = [HalfSpace]

data ClipResult = Outside | Across | Inside deriving Eq

clipPoint :: Bounds -> Vec3 -> ClipResult
clipPoint b x =
  let ds = [(n &. x) - d | (n, d) <- b]
  in case ds of
     _ | any (<0) ds -> Outside
     _ | all (>0) ds -> Inside
     otherwise -> Across

clipHalf :: Bounds -> Half -> ClipResult
clipHalf b h =
  let corners = [clipPoint b p | p <- snd $ realise h]
  in if all (/= Outside) corners
     then Inside
     else if all (/= Inside) corners
          then Outside
          else Across

clipHalf' :: Bounds -> Half -> Maybe (Either Half Half)
clipHalf' b h =
  case clipHalf b h of
  Inside -> Just $ Left h
  Outside -> Nothing
  Across -> Just $ Right h

-- | Give bounds and some halves, deflate the halves n times.
--   Return the resulting halves that are entirely within the bounds,
--   and the resulting halves that are on the bounds.
clipDeflate :: Bounds -> [Half] -> Integer -> ([Half], [Half])
clipDeflate b hs 0 = ([], hs)
clipDeflate b hs n = let (ins', ons') = partitionEithers $ mapMaybe (clipHalf' b) hs
                         (x, y) = clipDeflate b (deflate ons') (n-1)
                     in (foldl (\h _ -> deflate h) ins' [1..n] ++ x, y)
