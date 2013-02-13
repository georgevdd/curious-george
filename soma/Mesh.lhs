> module Mesh where

> import Data.Function (on)
> import Data.Map (fromList, keys, lookupIndex)
> import Data.Maybe (catMaybes, fromJust, listToMaybe)
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

> xAxisFaces :: Form -> [Face]
> xAxisFaces form = [Vec3 x y z |
>                    (z, layer)   <- zip [0..] (unform form),
>                    (y, row)     <- zip [0..] layer,
>                    (x, isFace)  <- zip [0..] (markFaces row),
>                    isFace]
>  where markFaces row = zipWith (/=) (' ':row) (row ++ [' '])

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

> axisFaces :: Axis -> Form -> [Face]
> axisFaces axis = map (vApplyRecipe $ fromXaxis Positive axis) .
>                  xAxisFaces .
>                  applyRecipe (toXaxis Positive axis)

> faceVerts axis (Vec3 x y z) = case axis of
>                               X -> [Vec3 x (y+y') (z+z') | (y', z') <- square]
>                               Y -> [Vec3 (x-x') y (z+z') | (x', z') <- square]
>                               Z -> [Vec3 (x-x') (y+y') z | (x', y') <- square]
>  where square = [(0,0), (0,1), (1,1), (1,0)]

> paintFace verts = listToMaybe . catMaybes $
>                   [paint vs axis | (vs, axis) <- [(xs, X), (ys, Y), (zs, Z)]]
>  where
>   (xs, ys, zs) = (map _1 verts, map _2 verts, map _3 verts)
>   paint vs axis = if all (== 0) vs then Just $ CubeFace Negative axis
>                   else if all (==3) vs then Just $ CubeFace Positive axis
>                   else Nothing

> axisInfo :: Recipe -> Axis -> Form -> [([Vert], FacePaint)]
> axisInfo recipe axis form = [(faceVerts axis face,
>                               FacePaint $ paintFace $ map (vApplyRecipe recipe)
>                               $ faceVerts axis face) |
>                              face <- axisFaces axis form]

> allFaces recipe f = concat [axisInfo recipe axis f | axis <- enumFrom X]

> shareVerts :: Ord k => [([k], t)] -> ([k], [([Int], t)])
> shareVerts faces = (keys vertMap,
>                     [([fromJust (lookupIndex v vertMap) | v <- verts], x) |
>                      (verts, x) <- faces])
>  where vertMap = fromList [(v, ()) | v <- concat (map fst faces)]

> mesh recipe = shareVerts . allFaces recipe . defaultForm
> allMeshes (Solution fs) = [(shape, mesh recipe shape) |
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
