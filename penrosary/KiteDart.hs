module KiteDart where
import Data.Vect.Double

type Scalar = Double

data Type = Kite | Dart deriving Show
data Half = Half Type Proj3 deriving Show

kite = [Half Kite one]
sun = [Half Kite (m .*. linear (rotMatrix2 $ n * 2/5 * pi)) |
             Half Kite m <- kite,
             n <- [0..4]]

phi :: Floating a => a
phi = (1 + sqrt 5) / 2

deflate1 :: Half -> [Half]
deflate1 (Half Kite m) = [Half t (foldr1 (.*.) l .*. m) | (t, l) <- [
  (Dart, [translation $ Vec2 (-1) 0,
          linear $ rotMatrix2 (-4/5 * pi),
          scaling $ Vec2 (2 - phi) (2 - phi)]),
  (Kite, [linear $ rotMatrix2 (-3/5 * pi),
          scaling $ Vec2 (phi - 1) (phi - 1),
          translation $ rotate2 (1/5 * pi) (Vec2 1 0)]),
  (Kite, [scaling $ Vec2 (phi - 1) (phi - 1),
          linear $ rotMatrix2 (3/5 * pi),
          translation $ rotate2 ((-1)/5 * pi) (Vec2 1 0)])
  ]]
deflate1 (Half Dart m) = [Half t (foldr1 (.*.) l .*. m) | (t, l) <- [
  (Dart, [translation $ rotate2 (-2/5 * pi) (Vec2 1 0),
          linear $ rotMatrix2 (4/5 * pi),
          scaling $ Vec2 (phi - 1) (phi - 1)]),
  (Kite, [linear $ rotMatrix2 pi,
          translation $ Vec2 1 0])
  ]]

deflate = concatMap deflate1

realise :: Half -> (Type, [Vec3])
realise (Half t m) = (t, [
 extendZero (project x m) | x <- [
  Vec2 1 0,
  rotate2 a (Vec2 1 0),
  Vec2 0 0,
  Vec2 0 0,
  rotate2 (-a) (Vec2 1 0),
  Vec2 1 0
 ]])
 where a = case t of Kite -> (pi/5)
                     Dart -> (3 * pi/5)
       project :: Vec2 -> Proj3 -> Vec2
       project x m = trim $ (extendWith 1 x :: Vec3) .* fromProjective m
