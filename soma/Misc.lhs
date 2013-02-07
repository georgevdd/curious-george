This is a dumping ground for bits and pieces that might migrate into more dedicated modules once they have more companions.

> module Misc where
>
> import Data.Bits
> import Data.List
> import Data.Function
> import Data.Ord
> import Data.Word

> -- | For example: histogram "aaabbcaa" == [('a',5),('b',2),('c',1)]
> histogram :: Ord a => [a] -> [(a, Int)]
> histogram xs = [(value, length bucket) |
>                 bucket@(value:_) <- (group . sort) xs]

> -- | Half an unzip.
> groupKey :: Eq a => [(a, b)] -> [(a, [b])]
> groupKey l = [(head xs, ys) |
>               (xs, ys) <- [unzip g |
>                            g <- groupBy ((==) `on` fst) l]]

> -- | Like @Data.List.partition@ but allows an impure predicate.
> --   For example:
> --     (existing, missing) <- partitionM doesFileExist filenames
> partitionM :: (Monad m, Functor m)
>            => (a -> m Bool)
>            -> [a]
>            -> m ([a], [a])
> partitionM f xs = fmap (uncurry ((,) `on` (map fst)) .
>                         partition snd .
>                         zip xs) $ mapM f xs

> -- | Sort and then group, both by the same function.
> groupSortBy :: Ord a => (b -> a) -> [b] -> [[b]]
> groupSortBy f = map (map fst) .
>                 groupBy ((==) `on` snd) .
>                 sortBy (comparing snd) .
>                 map (\x -> (x, f x))

> -- | Give a list of bitfields, group bit positions by equality in all values.
> --   For example, in the values xs = [13, 44, 54, 5], bits 1 and 4 always
> --   have the same value, so @dupBits xs@ will contain [1,4].
> dupBits :: (Bits a) => [a] -> [[Int]]
> dupBits xs = foldr refine [[0..bitSize (head xs) - 1]] xs where
>   refine x = concatMap (groupSortBy $ testBit x)

> -- | Data.Bits.popCount seems to hang on large Integers. This version doesn't.
> popCount' :: Integer -> Int
> popCount' 0 = 0
> popCount' n = popCount (fromInteger n :: Word64) + popCount' (n `shiftR` 64)

> -- | Enumerates the ways to choose n items from a list.
> choose 0 _ = [[]]
> choose n [] = []
> choose n (x:xs) = [x:xs' | xs' <- choose (n-1) xs] ++ choose n xs
