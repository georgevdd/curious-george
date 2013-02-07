> module Stats where

> import Data.Bits
> import Data.Function
> import Data.List
> import Data.Map ((!))
> import qualified Data.Map as Map
> import Data.Maybe
> import Data.Monoid
> import Data.Ord
> import Data.Word

> import Cube
> import Mesh
> import Misc
> import Word128

This counts how many squares cover the surface of a shape in the Soma cube.

A less brute-force way to arrive at the same numbers would be to see that:
 - each shape can be built up by starting with a single cube, and sticking successive cubes on by a single face;
 - the initial cube has six faces;
 - each subsequent cube adds four more faces (it brings six faces into play, but one of its own and one of the existing ones are stuck together and hidden).
So, the six four-cube shapes must all have 6 + 3*4 = 18 faces, and the one three-cube shape must have 6 + 2*4 = 14 faces.

> countFaces = length . allFaces undefined . defaultForm

That makes 122 faces in total. (In any solution, only 6*9 = 54 faces can go on the outside, so the other 68 must be inside the cube, in 34 pairs of adjacent faces.)

> totalFaces = sum [countFaces shape | shape <- enumFrom L]

The following defines an offset into a bitfield at which to find the bits for each face. Since there are 122 of them, they pack neatly into 128 bits.

> shapeBitOffset = Map.fromList $ foldl reserveBits [] (enumFrom L)
>  where
>   reserveBits [] shape = [(shape, 0)]
>   reserveBits os@((shape', o):_) shape = (shape, o + countFaces shape'):os

Given a predicate that selects faces, the corresponding subset of faces can be represented as 128 bits.

> markFaces :: (FacePaint -> Bool) -> [(Shape, [FacePaint])] -> Word128
> markFaces interesting painting = foldl setBit (fromInteger 0) bitsToSet
>  where
>   bitsForShape (shape, paints) = [shapeBitOffset!shape + i |
>                                   (i, paint) <- zip [0..] paints,
>                                   interesting paint]
>   bitsToSet = concatMap bitsForShape painting

'paintings' takes a solution and lists, for each shape, how the shape's faces would be coloured by that solution.

> paintings :: Solution -> [(Shape, [FacePaint])]
> paintings solution = [(shape, map snd faces) | (shape, (_, faces)) <- allMeshes solution]

Now there's enough information to work out how many of the shapes' faces are ever on the outside of the cube:

> totalEverExternalFaces = length [b | b <- showBinary everExternalBits, b == '1']
>  where
>   everExternalBits = foldr1 (.|.) [markFaces (isJust . cubeFace) $
>                                    paintings s | s <- solutions' RotationOnly]

This reveals that 87 of the 122 faces are ever on the outside. Since each face of a solution requires 9 faces of a shape, that means there is a theoretical limit of 87/9 = 9 disjoint solution faces that can simultaneously be assigned to the faces of the pieces.

Obviously, six disjoint solution faces can be simultaneously assigned to the faces of the pieces, just by choosing all the solution faces from the same solution. But that's not very interesting. The interesting problem is to have as many solution faces represented as possible, but where each one comes from a different solution.

In other words, the aim is to find as large a set of solutions as possible, such that there is some choice of one face from each solution having the property that the chosen faces all occupy disjoint sets of piece faces.

This function takes an assignment of cube faces to shape faces, and splits it into a separate list for each cube face.

> explodePainting :: [(Shape, [FacePaint])] -> [(CubeFace, [(Shape, [FacePaint])])]
> explodePainting p = [(cf, filterPaint cf p) |
>                      cf <- [CubeFace sign axis |
>                             sign <- [Negative, Positive],
>                             axis <- enumFrom X]]
>  where
>   filterPaint cf shapePaints = [(shape, [matchPaint cf fp | fp <- facePaints]) | (shape, facePaints) <- shapePaints]
>   matchPaint cf fp@(FacePaint cf') = if Just cf == cf' then fp else fp { cubeFace = Nothing }

Applying that splitting function to the face assignments of each solution in turn results in the following function. It produces a list of 6 * 480 = 2880 elements.

> faceCombos :: [((Solution, Int), CubeFace, Word128, [(Shape, [FacePaint])])]
> faceCombos = [((solution, n), cf, markFaces (isJust . cubeFace) fs, fs) |
>               (solution, n) <- zip (solutions' RotationOnly) [0..],
>               (cf, fs) <- explodePainting (paintings solution)]
> solutionIndex ((_, n), _, _, _) = n

Of course, there's some chance that a cube face from one solution will cover exactly the same shape faces as a cube face from another solution (albeit probably with different orientations). Ideally, a set of cube faces would not contain any cube face that is also formed by a different solution.

> distinctFaceCombos = nubBy ((==) `on` bitRep) faceCombos
> bitRep (_, _, w, _) = w

This reveals that there are only 1176 distinct cube faces. And even fewer will appear on only one solution. That's unsurprising in fact, because while symmetries of individual shapes are discounted, it's often the case that a solution will contain a subunit comprising two or more shapes, where the subunit has a symmetry. In such a solution, the subunit can be removed, transformed by its symmetry, and recombined, to give a new solution; any cube faces not affected by this will then be shared between the two solutions.

> groupedFaces = groupSortBy bitRep faceCombos
> faceFreq = histogram groupSizes
>  where
>   groupSizes = map length groupedFaces

The above function produces this histogram:
  [(1,619),(2,258),(3,104),(4,72),(5,39),(6,19),(7,22),(8,6),(9,6),(10,7),
   (11,2),(12,1),(13,4),(14,2),(15,1),(16,2),(18,2),(20,2),(25,1),(28,2),
   (30,2),(38,1),(46,1),(48,1)]

That is to say, there are 619 cube faces that can only be made by a single solution; there are 258 cube faces that can each only be made by two solutions, and so on ... up to one cube face that appears in 48 different solutions and another that appears in 46.

Here are the solution faces that appear only in a single solution:

> interestingFaceCombos = concat [g | g <- groupedFaces, length g == 1]

These can then be grouped by which solution they appear in:

> interestingSolutionFaces = sortBy (flip $ comparing length) $
>                            groupSortBy solutionIndex interestingFaceCombos

> interestingSolutionFreq = histogram $ map length interestingSolutionFaces

The above function produces this histogram:
  [(1,226),(2,131),(3,35),(4,4),(5,2)]

So, 226 solutions have one unique face each. But two solutions have five unique faces each!

> bitGroups = sortBy (comparing head) $
>             dupBits $ map bitRep interestingFaceCombos

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

> compressedFaceCombos = [(s, cf, compressRep b, p,
>                          conflicts (compressRep b)) |
>                         (s, cf, b, p) <- interestingFaceCombos]
>  where
>   conflicts :: Word64 -> Integer
>   conflicts b = foldl setBit 0
>                       [n |
>                        (n, b') <- zip [0..]
>                                       [compressRep $ bitRep fc |
>                                        fc <- interestingFaceCombos],
>                        b .&. b' /= 0]
