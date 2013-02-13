> module Mesh where

> import Control.Monad (msum)
> import Data.Function (on)
> import Data.Map (fromList, keys, lookupIndex)
> import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe)
> import Data.Ord (comparing)
> import Data.Vect

> import System.Process (runCommand)

> import Cube

> type Vert = Vec3
> type Face = Vec3

> instance Eq Vec3 where (==) = (==) `on` (destructVec3 . (:[]))
> instance Ord Vec3 where compare = comparing (destructVec3 . (:[]))

> vApplyRecipe :: Recipe -> Vec3 -> Vec3
> vApplyRecipe r = trim .
>                  (.* fromProjective r) .
>                  (extendWith 1 :: Vec3 -> Vec4)

> xAxisFaces :: Form -> [(Sign, Face)]
> xAxisFaces form = [(fromJust dir, o &+ Vec3 x y z) |
>                    (z, layer)   <- zip [0..] (unform form),
>                    (y, row)     <- zip [0..] layer,
>                    (x, dir)  <- zip [0..] (markFaces row),
>                    isJust dir]
>  where
>   markFaces row = zipWith faceDir (' ':row) (row ++ [' '])
>   faceDir ' ' ' ' = Nothing
>   faceDir ' '  x  = Just Negative
>   faceDir  x  ' ' = Just Positive
>   faceDir  _   _  = Nothing
>   o = Vec3 0 0.5 0.5

> data Sign = Negative | Positive deriving (Eq, Ord, Show)
> data CubeFace = CubeFace Sign Axis deriving (Eq, Ord)
> instance Show CubeFace
>  where show (CubeFace sign axis) = [head $ show axis, head $ show sign]
> newtype FacePaint = FacePaint { cubeFace :: Maybe CubeFace } deriving (Eq, Ord)
> instance Show FacePaint
>  where show (FacePaint (Just (CubeFace sign axis))) = [
>         '\"', head $ show axis, head $ show sign, '\"']
>        show (FacePaint Nothing) = "\"xx\""

> convXaxis :: Float -> Axis -> Recipe
> convXaxis _ X = one
> convXaxis n Y = exactRotation (rotMatrixZ $ -n*pi/2)
> convXaxis n Z = exactRotation (rotMatrixY $ n*pi/2)
> toXaxis Positive = convXaxis 1
> toXaxis Negative = (.*. exactRotation (rotMatrixY pi)) . toXaxis Positive
> fromXaxis Positive = convXaxis 3
> fromXaxis Negative = (exactRotation (rotMatrixY pi) .*.) . fromXaxis Positive

> axisFaces :: Axis -> Form -> [(Sign, Axis, Face)]
> axisFaces axis = map (\(sign, face) ->
>                       (sign,
>                        axis,
>                        vApplyRecipe (fromXaxis Positive axis) face)) .
>                  xAxisFaces .
>                  applyRecipe (toXaxis Positive axis)

> faceVerts sign axis (Vec3 x y z) = case axis of
>                                    X -> [Vec3 x (y+y') (z+z') | (y', z') <- square]
>                                    Y -> [Vec3 (x-x') y (z+z') | (x', z') <- square]
>                                    Z -> [Vec3 (x+x') (y+y') z | (x', y') <- square]
>  where square = (if sign == Negative then reverse else id)
>                 [(-0.5, -0.5), (0.5,-0.5), (0.5,0.5), (-0.5,0.5)]

> paintFace :: Recipe -> Face -> FacePaint
> paintFace recipe face = FacePaint $ msum $ map measure [X, Y, Z]
>  where
>   face' = vApplyRecipe recipe face
>   measure axis = case coord axis face' of
>                  3 -> Just (CubeFace Positive axis)
>                  0 -> Just (CubeFace Negative axis)
>                  _ -> Nothing
> coord X = _1
> coord Y = _2
> coord Z = _3

> allFaces form = concat [axisFaces axis form | axis <- enumFrom X]

> allInfo :: (Face -> a) -> Form -> [([Vert], a)]
> allInfo painter form = [(faceVerts sign axis face, painter face) |
>                         (sign, axis, face) <- allFaces form]

> shareVerts :: Ord k => [([k], t)] -> ([k], [([Int], t)])
> shareVerts faces = (keys vertMap,
>                     [([fromJust (lookupIndex v vertMap) | v <- verts], x) |
>                      (verts, x) <- faces])
>  where vertMap = fromList [(v, ()) | v <- concat (map fst faces)]

> mesh painter = shareVerts . allInfo painter . defaultForm
> allMeshes (Solution fs) = [(shape, mesh (paintFace recipe) shape) |
>                            (shape, recipe, _) <- fs]

> type PyVert = (Int, Int, Int)
> pyVert :: Vert -> PyVert
> pyVert (Vec3 x y z) = (round x, round y, round z)
> pyMesh (vs, x) = (map pyVert vs, x)
> writeMeshes :: [(Shape, ([Vert], [([Int], FacePaint)]))] -> IO ()
> writeMeshes ms = writeFile "shapes.txt" $ show [(show shape, pyMesh m) |
>                                                 (shape, m) <- ms]

> pyRecipe recipe = (tfm, pyVert origin)
>  where
>   tfm = [[mat j i | j <- [0..2]] | i <- [0..2]]
>   mat i j = idx j $ idx i $ fromProjective recipe
>   idx :: HasCoordinates v e => Int -> v -> e
>   idx = ([_1, _2, _3, _4]!!)

>   origin  = vApplyRecipe recipe zero

> pySolution (Solution fs) = [(show shape, pyRecipe recipe) |
>                             (shape, recipe, _) <- fs]

> writeSolutions ss = writeFile "pysolutions.txt" $
>                     show [pySolution solution | solution <- ss]

> importIntoBlender = do
>   _ <- runCommand $ unwords ["blender", "-P", "blender.py"]
>   return ()

> testMeshes = do
>   writeMeshes $ allMeshes (head solutions)
>   writeSolutions solutions
>   importIntoBlender
>   return ()
