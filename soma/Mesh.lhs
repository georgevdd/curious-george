The representation of solutions in the Cube module having been reworked, it's now possible to export data about the shapes and their positions in each solution, in a form that Blender can then load via a short Python script.

Here's the Haskell side of things:

> module Mesh where

> import Data.Map (fromList, keys, lookupIndex)
> import Data.Maybe (catMaybes, fromJust, listToMaybe)

> import System.Process (runCommand)

> import Cube

All the logic for moving a shape around within the cube has equivalents for moving a vertex or face around inside the cube. This will be useful for determining which faces of a shape end up on which faces of the cube in a particular solution.

> vApplyR :: Rotation -> (Int, Int, Int) -> (Int, Int, Int)
> vApplyR (Rotation a n) = times n (r a)
>  where
>   r X (x, y, z) = (x, 3-z, y)
>   r Y (x, y, z) = (z, y, 3-x)
>   r Z (x, y, z) = (3-y, x, z)

> vApplyT :: Translation -> (Int, Int, Int) -> (Int, Int, Int)
> vApplyT (Translation (x', y', z')) (x, y, z) = (x+x', y+y', z+z')

> vApplyRecipe (maj, min, shift) = vApplyR maj . vApplyR min . vApplyT shift

In effect, this function scans all the possible face positions, and determines where the cube is occupied by a form on one side along the X axis but not on the other. These will be the places where the form will need an X-face (i.e. one parallel to the YZ plane). (Locations for the Y- and Z-faces can be found by rotating the form appropriately first.)

> faceCoords :: Form -> [(Int, Int, Int)]
> faceCoords form = [(x, y, z) |
>                    (z, layer)   <- zip [0..] (unform form),
>                    (y, row)     <- zip [0..] layer,
>                    (x, isFace)  <- zip [0..] (markFaces row),
>                    isFace]
>  where markFaces row = zipWith (/=) (' ':row) (row ++ [' '])

A few new data types are needed, to record which side of the solved cube a face belongs to. The 'Show' instances for these have been carefully chosen to make parsing in Python easy.

> data Sign = Negative | Positive deriving (Show)
> data CubeFace = CubeFace Sign Axis
> instance Show CubeFace
>  where show (CubeFace sign axis) = [head $ show axis, head $ show sign]
> newtype FacePaint = FacePaint (Maybe CubeFace)
> instance Show FacePaint
>  where show (FacePaint (Just (CubeFace sign axis))) = [
>         '\"', head $ show axis, head $ show sign, '\"']
>        show (FacePaint Nothing) = "\"xx\""

> type Vert = (Int, Int, Int)

> type Face = (Int, Int, Int)

Now everything necessary is present, to be able to determine the locations of a form's faces for any axis. Note that applyR and vApplyR are used to represent a transformation and its inverse, respectively.

> axisFaces :: Axis -> Form -> [Face]
> axisFaces axis = map (vApplyR $ r axis 3) .
>                  faceCoords .
>                  applyR (r axis 1)
>  where
>   r X _ = Rotation X 0
>   r Y n = Rotation Z n
>   r Z n = Rotation Y n

Given a face on an axis, this function determines the locations of its vertices.

> faceVerts axis (x, y, z) = case axis of
>                              X -> [(x, y+y', z+z') | (y', z') <- square]
>                              Y -> [(x+x', y, z+z') | (x', z') <- square]
>                              Z -> [(x-x', y+y', z) | (x', y') <- square]
>  where square = [(0,0), (0,1), (1,1), (1,0)]

Knowing where a face's vertices lie (after transformation) is enough to determine which face of the cube (if any) it lies on.

> paintFace verts = listToMaybe . catMaybes $
>                   [paint vs axis | (vs, axis) <- [(xs, X), (ys, Y), (zs, Z)]]
>  where
>   (xs, ys, zs) = unzip3 verts
>   paint vs axis = if all (== 0) vs then Just $ CubeFace Negative axis
>                   else if all (==3) vs then Just $ CubeFace Positive axis
>                   else Nothing

Finally, all these things are brought together to give a way to describe how to position and colour all of a shape's faces to suit the space it occupies in a solution.

> axisInfo :: Recipe -> Axis -> Form -> [([Vert], FacePaint)]
> axisInfo recipe axis form = [(faceVerts axis face,
>                               FacePaint $ paintFace $ map (vApplyRecipe recipe) $ faceVerts axis face) |
>                              face <- axisFaces axis form]


> allFaces recipe f = concat [axisInfo recipe axis f | axis <- enumFrom X]

This little helper takes a list of faces (each having a list of vertices) and pulls all the vertices out into a separate (deduplicated) list, replacing them in the original data structure by indices into that vertex list.

> shareVerts :: Ord k => [([k], t)] -> ([k], [([Int], t)])
> shareVerts faces = (keys vertMap,
>                     [([fromJust (lookupIndex v vertMap) | v <- verts], x) |
>                      (verts, x) <- faces])
>  where vertMap = fromList [(v, ()) | v <- concat (map fst faces)]

Because the colouring of a shape depends on the choice of solution, the mesh can't be generated without specifying a recipe for the shape.

> mesh recipe = shareVerts . allFaces recipe . defaultForm
> allMeshes (Solution fs) = [(show shape, mesh recipe shape) | (shape, recipe, _) <- fs]

The file emitted here is one big list of mesh data. Luckily, 'show' for Haskell's built-in types and 'eval' in Python are near enough inverses that, with some careful implementations of 'Show' (as noted above) it's possible to avoid writing a parser for it at all.

> writeMeshes = writeFile "shapes.txt" $ show $ allMeshes (head solutions)

This rather ugly function describes the two rotations and a translation that compose a Recipe, in a form that can easily be parsed in Python.

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
>                  show [pySolution solution | solution <- solutions]

Lastly, a test command glues everything together: writing out the data, then loading it into Blender.

> testMeshes = do
>   writeMeshes
>   writeSolutions
>   _ <- runCommand $ unwords ["blender", "-P", "blender.py"]
>   return ()
