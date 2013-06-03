> module Jumble where

> import Data.List (intercalate, lookup)

> import Cube
> import Mesh
> import Stats

> getSF (_, s, f, _, _) = (s, f)

> overlaps (shape, faces) (shape', faces')
>              | shape == shape' = length [() |
>                                          (x, y) <- zip faces faces', x && y]
> overlaps _ _ = error "Faces don't match."

> clashFactor (s, _) (s', f') =
>     let faceShapePaintings   =            explodePainting $ paintings s
>         Just shapePaintings' = lookup f' (explodePainting $ paintings s')
>     in maximum [sum [overlaps x y |
>                      (x, y) <- zip shapePaintings shapePaintings'] |
>                 (_, shapePaintings) <- faceShapePaintings]

> allClashes =  [[clashFactor a b | b <- sfs] | a <- sfs]
>  where sfs = map getSF compressedFaceInfos

This can be used to emit a file where the Nth row describes how the Nth
interesting solution faces clashes with each of the other interesting solution
faces.

> writeClashes = writeFile "clashes.txt" $
>                          unlines [intercalate "," (map show row) |
>                                   row <- allClashes]


The following solution took about 3.5 minutes to find.

> chosenCombo = map (compressedFaceInfos !!)
>                   [22, 48, 114, 138, 246, 326, 358, 415, 482, 576]

