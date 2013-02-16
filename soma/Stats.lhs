> module Stats where

> import Control.Monad
> import Control.Parallel
> import Data.Bits
> import Data.Function
> import Data.List
> import Data.Map ((!))
> import qualified Data.Map as Map
> import Data.Maybe
> import Data.Monoid
> import Data.Ord
> import Data.Vect hiding (transpose)
> import Data.Word

> import Cube hiding (main)
> import Mesh hiding (testMeshes)
> import Misc
> import Word128

This counts how many squares cover the surface of a shape in the Soma cube.

A less brute-force way to arrive at the same numbers would be to see that:
 - each shape can be built up by starting with a single cube, and sticking successive cubes on by a single face;
 - the initial cube has six faces;
 - each subsequent cube adds four more faces (it brings six faces into play, but one of its own and one of the existing ones are stuck together and hidden).
So, the six four-cube shapes must all have 6 + 3*4 = 18 faces, and the one three-cube shape must have 6 + 2*4 = 14 faces.

> countFaces = length . allFaces . defaultForm

That makes 122 faces in total. (In any solution, only 6*9 = 54 faces can go on the outside, so the other 68 must be inside the cube, in 34 pairs of adjacent faces.)

> totalFaces = sum [countFaces shape | shape <- enumFrom L]

The following defines an offset into a bitfield at which to find the bits for each face. Since there are 122 of them, they pack neatly into 128 bits.

> shapeBitOffset = Map.fromList $ foldl reserveBits [] (enumFrom L)
>  where
>   reserveBits [] shape = [(shape, 0)]
>   reserveBits os@((shape', o):_) shape = (shape, o + countFaces shape'):os

Given a predicate that selects faces of shapes, the corresponding subset of faces can be represented as 128 bits.

> markingsAsBits :: [(Shape, [Bool])] -> Word128
> markingsAsBits markings = foldl setBit (fromInteger 0) bitsToSet
>  where
>   bitsForShape (shape, marking) = [shapeBitOffset!shape + i |
>                                    (i, marked) <- zip [0..] marking,
>                                    marked]
>   bitsToSet = concatMap bitsForShape markings

> mapFaces :: (a -> b) -> [(Shape, [a])] -> [(Shape, [b])]
> mapFaces f = map (\(shape, faces) -> (shape, map f faces))

> markFaces :: (a -> Bool) -> [(Shape, [a])] -> Word128
> markFaces f = markingsAsBits . mapFaces f

'paintings' takes a solution and lists, for each shape, how the shape's faces would be coloured by that solution.

> paintings :: Solution -> [(Shape, [FacePaint])]
> paintings solution = [(shape, map snd faces) |
>                       (shape, (_, faces)) <- allMeshes solution]

Now there's enough information to work out how many of the shapes' faces are ever on the outside of the cube:

> totalEverExternalFaces = popCount everExternalBits
>  where
>   everExternalBits = foldr1 (.|.) [markFaces (isJust . cubeFace) $
>                                    paintings s | s <- solutions' RotationOnly]

This reveals that 90 of the 122 faces are ever on the outside. Since each face of a solution requires 9 faces of a shape, that means there is a theoretical limit of 90/9 = 10 disjoint solution faces that can simultaneously be assigned to the faces of the pieces.

Obviously, six disjoint solution faces can be simultaneously assigned to the faces of the pieces, just by choosing all the solution faces from the same solution. But that's not very interesting. The interesting problem is to have as many solution faces represented as possible, but where each one comes from a different solution.

In other words, the aim is to find as large a set of solutions as possible, such that there is some choice of one face from each solution having the property that the chosen faces all occupy disjoint sets of piece faces.

This function takes an assignment of cube faces to shape faces, and splits it into a separate list describing, for each cube face, which shape faces it comprises.

> explodePainting :: [(Shape, [FacePaint])] -> [(CubeFace, [(Shape, [Bool])])]
> explodePainting p = [(cf, filterPaint cf p) |
>                      cf <- [CubeFace sign axis |
>                             sign <- [Negative, Positive],
>                             axis <- enumFrom X]]
>  where
>   filterPaint cf shapePaints = [(shape, [match cf fp | fp <- facePaints]) |
>                                 (shape, facePaints) <- shapePaints]
>   match cf (FacePaint cf') = (Just cf == cf')

Applying that splitting function to the face assignments of each solution in turn results in the following function. It produces a list of 6 * 480 = 2880 elements.

> faceInfos :: [((Solution, Int), CubeFace, Word128, [(Shape, [Bool])])]
> faceInfos = [((solution, n), cf, markingsAsBits fs, fs) |
>               (solution, n) <- zip (solutions' RotationOnly) [0..],
>               (cf, fs) <- explodePainting (paintings solution)]
> solutionIndex ((_, n), _, _, _) = n
> bitRep        (_,      _, w, _) = w

Of course, there's some chance that a cube face from one solution will cover exactly the same shape faces as a cube face from another solution (albeit probably with different orientations). Ideally, a set of cube faces would not contain any cube face that is also formed by a different solution.

> distinctFaceInfos = nubBy ((==) `on` bitRep) faceInfos

This reveals that there are only 1176 distinct cube faces. And even fewer will appear on only one solution. That's unsurprising in fact, because while symmetries of individual shapes are discounted, it's often the case that a solution will contain a subunit comprising two or more shapes, where the subunit has a symmetry. In such a solution, the subunit can be removed, transformed by its symmetry, and recombined, to give a new solution; any cube faces not affected by this will then be shared between the two solutions.

> groupedFaces = groupSortBy bitRep faceInfos
> faceFreq = histogram groupSizes
>  where
>   groupSizes = map length groupedFaces

The above function produces this histogram:
  [(1,619),(2,258),(3,104),(4,72),(5,39),(6,19),(7,22),(8,6),(9,6),(10,7),
   (11,2),(12,1),(13,4),(14,2),(15,1),(16,2),(18,2),(20,2),(25,1),(28,2),
   (30,2),(38,1),(46,1),(48,1)]

That is to say, there are 619 cube faces that can only be made by a single solution; there are 258 cube faces that can each only be made by two solutions, and so on ... up to one cube face that appears in 48 different solutions and another that appears in 46.

Here are the solution faces that appear only in a single solution:

> interestingFaceInfos = concat [g | g <- groupedFaces, length g == 1]

These can then be grouped by which solution they appear in:

> interestingSolutionFaces = sortBy (flip $ comparing length) $
>                            groupSortBy solutionIndex interestingFaceInfos

> interestingSolutionFreq = histogram $ map length interestingSolutionFaces

The above function produces this histogram:
  [(1,226),(2,131),(3,35),(4,4),(5,2)]

So, 226 solutions have one unique face each. But two solutions have five unique faces each!

Although the cube faces between them cover 90 different shape faces, there is some redundant information being encoded, because a shape has several coplanar faces, and when a cube face is assigned to a shape face, it must be assigned to all coplanar faces on the same shape. For example, when a cube face contains one of the L-shaped sides of shape L, it must contain all four of the shape faces comprised by that L-shaped side.

Eliminating this redundant information could be done earlier (at the face painting stage) by comparing faces' geometry, but here will do just as well and keeps things simple, by avoiding the need for a representation of a mapping from cell face to shape face.

By removing this redundancy, the cube-face-to-shape-face mapping can be reduced to just 42 bits per cube face, which is good because it will fit in a native 64-bit word now.

> bitGroups = sortBy (comparing head) $
>             dupBits $ map bitRep interestingFaceInfos

> compressRep :: Word128 -> Word64
> compressRep w = foldl (mapBit w) 0 bitMap
>  where
>   mapBit :: (Bits a, Bits b) => a -> b -> (Int, Int) -> b
>   mapBit w w' (outBit, inBit) = if (testBit w inBit)
>                                 then setBit w' outBit
>                                 else w'
>   bitMap = zip [0..] (map head bitGroups)

> uncompressRep :: Word64 -> Word128
> uncompressRep w' = foldl (unmapBit w') 0 bitUnmap
>  where
>   unmapBit :: (Bits a, Bits b) => a -> b -> (Int, [Int]) -> b
>   unmapBit w' w (inBit, outBits) = if (testBit w' inBit)
>                                    then foldl setBit w outBits
>                                    else w
>   bitUnmap = zip [0..] bitGroups

The following working form now contains, for each interesting solution face, a deduplicated representation of the shape faces that it uses, and also an Integer describing which of the other 618 interesting solution faces cannot appear in conjunction with it.

> compressedFaceInfos = [(n, s, cf, compressRep b, p) |
>                        (n, ((s, _), cf, b, p)) <- zip [0..]
>                                                       interestingFaceInfos]

Each cube face needs 9 cell faces.

> targetComboSize = totalEverExternalFaces `div` 9

The above working provides the basis for a naive depth-first search for solutions.

> search partialSolution n
>        usedFaceBits
>        bestN
>        infos =
>     if n > bestN
>     then let (restSolutions, restBestN) = rest n infos
>          in (partialSolution : restSolutions, max n restBestN)
>     else rest bestN infos
>  where
>   rest bestN' [] = ([], bestN')
>   rest bestN' (info@(_,_,_,faceBits,_):infos) =
>     if (usedFaceBits .&. faceBits == 0)
>     then let (withThisInfo, bestN'W) = search (info:partialSolution) (n+1)
>                                               (usedFaceBits .|. faceBits)
>                                               bestN'
>                                               infos
>              (withoutThisInfo, bestN'') = search partialSolution n
>                                                  usedFaceBits
>                                                  bestN'W
>                                                  infos
>          in (withThisInfo ++ withoutThisInfo, bestN'')
>     else search partialSolution n
>                 usedFaceBits
>                 bestN'
>                 infos

> run' minLength = search [] 0 0 (minLength-1)
> run = run' targetComboSize compressedFaceInfos

> chosenCombo' = head $ fst run

Of course, this simple approach runs straight into the combinatrial explosion of having to choose ten elements from a set of 619; considering each possibility at a time is going to take longer than a lifetime. Luckily, there are other ways to attack the problem and once there's a solution it can be plugged in quickly.

The following was found very quickly (that's to say, in less than a second) with a linear constraint solver. Thanks to Google's math mailing list for that suggestion.

> chosenCombo = map (compressedFaceInfos!!)
>                   [68, 139, 152, 208, 234, 336, 363, 386, 487, 513]

The naive search may not be efficient enough for finding a solution, but it's suitable for checkin a solution. This confirms that the selection made by the linear constraint solver does have the properties required.

> chosenComboIsValid = (not . null . fst) $ run' targetComboSize chosenCombo

Having found a set of ten solution faces that completely cover the shape faces, it's time to condense those faces into one data set that describes, for each shape face, which solution face it belongs in and where in the solution face it appears. That will be enough to asssign a texture to the shape face, and also texture coordinates.

> type ShapeFaceData = Maybe (Int, Recipe)

This elaborate list comprehension fits together all the exploded face information from earlier, into a single structure that collects the facts for each face of each shape.

> implodePainting :: [(Int, Solution, [(Shape, [Bool])])]
>                 -> [(Shape, [ShapeFaceData])]
> implodePainting explodedMarkings = [
>     (shape, superpose paintGroups) |
>     (shape, paintGroups) <- [head $ groupKey grp |
>                              grp <- groupSortBy fst .
>                              concat $ explodedShapeFaceDatas]]
>  where
>   superpose :: [[ShapeFaceData]] -> [ShapeFaceData]
>   superpose = map msum . transpose
>   explodedShapeFaceDatas :: [[(Shape, [ShapeFaceData])]]
>   explodedShapeFaceDatas = [attachMaterials (i, solution, shapeFacePaints) |
>                            (i, solution, shapeFacePaints) <- explodedMarkings]

The painting condensation logic relies on this helper function, whose purpose is to multiply solution face information out onto each face that appears on the solution face.

> attachMaterials :: (Int, Solution, [(Shape, [Bool])]) ->
>                                    [(Shape, [ShapeFaceData])]
> attachMaterials (cubeFaceNumber, solution, shapeFacePaints) = [
>  (shape, [if paint then Just (cubeFaceNumber, recipe) else Nothing |
>           paint <- facePaints]) |
>  (shape, recipe, facePaints) <- merge solution shapeFacePaints]
>   where merge (Solution s) (facePaints) = [
>          (checkEqual "Yikes!" shape shape', recipe, paints) |
>          ((shape, recipe, _), (shape', paints) )<- zip s facePaints]

> checkEqual msg s1 s2 = if s1 == s2 then s1 else error msg

The solution information can now be split into two results.

The first result is an association of solution to cube face, which allows for successive face solution animations to point the same way, even though different chosen solution faces have different orientations.

The second result describes, for each face of each shape, which solution it belongs to (if any) and the position of the shape in that solution (if any), which is necessary for assigning UV coordinates.

> implodeCombo combo = (solutionToCubeFace, implodePainting nsps)
>  where (solutionToCubeFace, nsps) = unzip [((s, cf), (n, s, p)) |
>                                            (n, (_, s, cf, _, p)) <-
>                                            zip [0..] combo]

A couple of small helper functions ease export to a Python-readable form.

> pyVec2 (Vec2 x y) = (x, y)
> pyMaterial Nothing       = "Face" ++ "X"
> pyMaterial (Just (n, _)) = "Face" ++ show n

> painter faceMaterials shape = fromJust . flip lookup faceInfo
>  where 
>   faceInfo = [(face, (pyMaterial mat, uvs mat (faceVerts sign axis face))) |
>               ((sign, axis, face), mat) <-
>               zip (allFaces $ defaultForm shape)
>                   faceMaterials]

Always-internal shape faces don't add up to nice shapes, so they can each display a full texture.

>   uvs Nothing _ = [(0,0),(0,1),(1,1),(1,0)]

For sometimes-external shape faces, the UV coordinates are chosen by transforming the face's vertices into their position in the solution to which the shape face belongs, then seeing whether the vertices appear on the corresponding cube face. At this point there's no information about which cube face it's meant to be, so that is reconstructed from the vertices' normal. (It ought to be possible to keep the cube face information up to this point, then use 'toXaxis' to avoid the normal calculation. But that's for another time.)

>   uvs (Just (_, recipe)) verts = [pyVec2 $ extractUvs vert' &* (1/3) |
>                                   vert' <- verts']
>     where
>       verts' = map (vApplyRecipe recipe) verts  -- into solution space
>       extractUvs = (apply . extractor) verts'
>   apply :: (Vec3 -> Float, Vec3 -> Float) -> Vec3 -> Vec2
>   apply (exU, exV) vec = Vec2 (exU vec) (exV vec)

It's perhaps possible to simplify this mapping, but how is not obvious.

>   extractor [a,b,c,_] =  ex $ pyVert uvNormal
>    where uvNormal = (b &- a) &^ (c &- b)
>          ex (1, 0, 0) = (p _2, p _3)
>          ex (0, 1, 0) = (n _1, p _3)
>          ex (0, 0, 1) = (p _2, n _1)
>          -- For negative normals, just flip the V coordinate
>          ex (a, b, c) = let (eu, ev) = ex (-a, -b, -c) in (p eu, n ev)
>          n = ((3-) .)
>          p = id

> testMeshes = do
>   let (solutionToCubeFace, shapeFaces) = implodeCombo chosenCombo
>   writeMeshes [(shape, mesh (painter faceInfos shape) shape) |
>                (shape, faceInfos) <- shapeFaces]
>   writeSolutions [Solution [(shape, recipe .*. toXaxis sign axis, bitmap) |
>                             (shape, recipe, bitmap) <- s] |
>                   (Solution s, CubeFace sign axis) <- solutionToCubeFace]
>   importIntoBlender

> main = testMeshes
