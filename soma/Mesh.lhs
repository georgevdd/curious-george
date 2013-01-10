> module Mesh where

> import Data.Map (fromList, keys, lookupIndex)
> import Data.Maybe (fromJust)

> import System.Process (runCommand)

> import Cube


> vApplyR :: Rotation -> (Int, Int, Int) -> (Int, Int, Int)
> vApplyR (Rotation a n) = times n (r a)
>  where
>   r X (x, y, z) = (x, 3-z, y)
>   r Y (x, y, z) = (z, y, 3-x)
>   r Z (x, y, z) = (3-y, x, z)

> vApplyT :: Translation -> (Int, Int, Int) -> (Int, Int, Int)
> vApplyT (Translation (x', y', z')) (x, y, z) = (x+x', y+y', z+z')

> vApplyRecipe (maj, min, shift) = vApplyR maj . vApplyR min . vApplyT shift


> faceCoords :: [[[Char]]] -> [(Int, Int, Int)]
> faceCoords layers = [(x, y, z) |
>                      (z, layer)   <- zip [0..] layers,
>                      (y, row)     <- zip [0..] layer,
>                      (x, isFace)  <- zip [0..] (markFaces row),
>                      isFace]
>  where markFaces row = zipWith (/=) (' ':row) (row ++ [' '])


> axisFaces :: Axis -> Form -> [(Int, Int, Int)]
> axisFaces axis = map (vApplyR $ r axis 3) .
>                  faceCoords . unform .
>                  applyR (r axis 1)
>  where
>   r X _ = Rotation X 0
>   r Y n = Rotation Z n
>   r Z n = Rotation Y n

> faceVerts axis (x, y, z) = case axis of
>                              X -> [(x, y+y', z+z') | (y', z') <- square]
>                              Y -> [(x+x', y, z+z') | (x', z') <- square]
>                              Z -> [(x-x', y+y', z) | (x', y') <- square]
>  where square = [(0,0), (0,1), (1,1), (1,0)]

> axisVerts :: Axis -> Form -> [[(Int, Int, Int)]]
> axisVerts axis = map (faceVerts axis) . axisFaces axis

> allFaces f = concat [axisVerts axis f | axis <- enumFrom X]

> shareVerts faces = (keys vertMap,
>                     [[fromJust (lookupIndex v vertMap) | v <- face] |
>                      face <- faces])
>  where vertMap = fromList [(v, ()) | v <- concat faces]

> mesh = shareVerts . allFaces . defaultForm
> allMeshes = [(show shape, mesh shape) | shape <- enumFrom L]
> writeMeshes = writeFile "shapes.txt" $ show allMeshes

> pyRecipe recipe@(maj, min, shift) = ((roll, pitch, heading), location)
>  where
>   location = vApplyRecipe recipe (0, 0, 0) 
>   heading =
>     case maj of Rotation X _ -> error "X should only appear as a minor axis"
>                 Rotation Y _ -> 0
>                 Rotation Z n -> quarterTurns n
>   pitch =
>     case maj of Rotation X _ -> error "X should only appear as a minor axis"
>                 Rotation Y n -> quarterTurns n
>                 Rotation Z _ -> 0
>   roll =
>     case min of Rotation X n -> quarterTurns n
>                 _            -> error "Only X should appear as a minor axis"
>   quarterTurns n = (fromIntegral n) * pi/2

> pySolution (Solution fs) = [(show shape, pyRecipe recipe) |
>                             (shape, recipe, _) <- fs]

> writeSolutions = writeFile "pysolutions.txt" $
>                  show [pySolution solution | solution <- take 1 solutions]

> testMeshes = do
>   writeMeshes
>   writeSolutions
>   _ <- runCommand $ unwords ["blender", "-P", "blender.py"]
>   return ()





-- > paintFace :: Axis -> (Int, Int, Int) -> Maybe CubeFace
-- > paintFace axis (0, _, _) = Just $ CubeFace Negative axis
-- > paintFace axis (3, _, _) = Just $ CubeFace Positive axis
-- > paintFace _ _ = Nothing


-- > yy (Solution fs) = [[(shape, vApplyRecipe recipe face) | face <- (axisFaces axis $ defaultForm shape)] |
-- >                      (shape, recipe, _) <- fs,
-- >                      axis <- enumFrom X]




-- > data Sign = Negative | Positive deriving (Show)
-- > data CubeFace = CubeFace Sign Axis deriving (Show)

-- > paintFaces :: Shape -> Recipe -> [a]
-- > paintFaces shape recipe = undefined
