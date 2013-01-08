> import Data.Map (fromList, keys, lookupIndex)
> import Data.Maybe (fromJust)

> import Cube

> faceCoords :: [[[Char]]] -> [(Int, Int, Int)]
> faceCoords layers = [(x, y, z) |
>                      (z, layer)   <- zip [0..] layers,
>                      (y, row)     <- zip [0..] layer,
>                      (x, isFace)  <- zip [0..] (markFaces row),
>                      isFace]
>  where markFaces row = zipWith (/=) (' ':row) (row ++ [' '])

> faceVerts (x, y, z) = [(x, y+y', z+z') |
>                        (y', z') <- [(0,0), (0,1), (1,1), (1,0)]]

> axisFaces :: Axis -> Form -> [[(Int, Int, Int)]]
> axisFaces axis = map (map $ r' axis) .
>                  map faceVerts . faceCoords . unform .
>                  applyR (r axis)
>  where
>   r  X = Rotation X 0
>   r  Y = Rotation Z 1
>   r  Z = Rotation Y 1
>   r' X p = p
>   r' Y (y, x, z) = (x, 3-y, z)
>   r' Z (z, y, x) = (3-x, y, z)

> allFaces f = concatMap (flip axisFaces f) (enumFrom X)

> shareVerts :: Ord a => [[a]] -> ([a], [[Int]])
> shareVerts faces = (keys vertMap, [[fromJust (lookupIndex v vertMap) | v <- face] | face <- faces])
>  where vertMap = fromList [(v, ()) | v <- concat faces]

> mesh = shareVerts . allFaces . defaultForm
> allMeshes = [(show shape, mesh shape) | shape <- enumFrom L]
