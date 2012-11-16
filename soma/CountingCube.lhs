In a <a href="http://future-indicative.blogspot.com/2012/10/mike-has-cube-like-this.html">previous post</a> I developed a backtracking algorithm for finding solutions of a <a href="http://wikipedia.org/wiki/Soma_cube">Soma cube</a>. It was good to discover that an optimized version of the program would find all the solutions in only 1.2s.

Later, I began to wonder how much of the speed was down to being able to prune the solution space effectively, versus simply being able to check each candidate solution very quickly. A revised version of the program emits statistics to help find out.

> module CountingCube where
> import Cube hiding (main)
> import Data.Bits
> import Data.Monoid (mappend, mempty)
> import Data.List (intercalate, sortBy)
> import Data.Ord (comparing)
> import Data.Word
> import System.IO

In this version, the computation returns not just a list of solutions, but also a list of details of how much work was done for each additional shape folded into the partial solution. (The work list has to be reversed once it is complete because it is built up backwards.)

> countedSolutions = (map fst pbs, reverse work) where
>     (pbs, work) = foldr addForm ([(mempty, 0)], []) combos

For each shape, all the forms of that shape have to be tried against all the partial solutions built so far, so the number of candidates tried will be the product of those two lists' lengths. The number of candidates <i>kept</i> is just the number of partial solutions resulting from this step of the fold.

> addForm form (partialSolutions, work) = (partialSolutions', work') where
>     partialSolutions' = [(ps `mappend` f, psb .|. fb) |
>                          (f, fb) <- form,
>                          (ps, psb) <- partialSolutions,
>                          psb .&. fb == 0]
>     work' = (tried, kept) : work
>     tried = length form * length partialSolutions
>     kept = length partialSolutions'

As before, the program prints out all the solutions, separated by newlines. But this time it also describes how much work was done.

> printSolutions (solutions, work) = do
>   putStrLn $ intercalate "\n" [show s | s <- solutions]
>   hPutStrLn stderr $ unlines $ [
>                  "Possibilities (explored, kept) at each stage:"
>                 ] ++ map (("  " ++) . show) work ++ [
>                  "Total possibilities explored: " ++ show (sum (map fst work)),
>                  "Total possibilities kept: " ++ show (sum (map snd work))
>                  ]

> main = printSolutions countedSolutions

How did that turn out?

~~~
$ ghc --make -main-is CountingCube -O2 CountingCube.lhs
[2 of 2] Compiling CountingCube     ( CountingCube.lhs, CountingCube.o )
Linking CountingCube ...
$ time ./CountingCube > solutions.txt 
Possibilities (explored, kept) at each stage:
  (64,64)
  (6144,2544)
  (244224,38448)
  (5536512,805584)
  (58002048,1817232)
  (130840704,552384)
  (2209536,240)
Total possibilities explored: 196839232
Total possibilities kept: 3216496


real	0m3.594s
user	0m3.374s
sys	0m0.212s
~~~

Because the fold is right-associative, it starts with the last shape in the list: Y. As can be seen, the Y was tried in all 64 positions in which it could appear, and was found to fit in all of them - just as expected, given that no shapes had been added ahead of it. The next shape to be tried fitted in the position tried 2544 out of 6144 times - less than half of the time.


Overall, the number of moves attempted was 196,839,232. The entire solution space (up to rotations and reflections) has 268,435,456 possibilities, but each of seven shapes has to be placed for each one of those, so a completely stupid non-backtracking implementation would try 1,879,048,192 moves. So pruning of the solution space like this avoids about 90% of the work.

But it gets better. While writing a version of the same algorithm in C++ (of which more later) I noticed that the total work done was different, even though the number of solutions ended up being the same, simply because the shapes were being considered in a different order. Of course ... so it would be better to try to place the shapes having fewer forms first, because they constrain the solution space more so there should be less backtracking to do.

Note that `smarterSolutions` here is identical to `countedSolutions` above, except that it sorts the input first.

> smarterSolutions = (map fst pbs, reverse work) where
>     (pbs, work) = foldr addForm
>                   ([(mempty, 0)], [])
>                   (reverse $ sortBy (comparing length) combos)

> main' = printSolutions smarterSolutions

`GHC --make` unfortunately doesn't seem to tell the difference between programs built with different `-main-is` settings, so the previous run's outputs need to be deleted first.

~~~
$ rm CountingCube CountingCube.o CountingCube.hi
$ ghc --make -main-is "CountingCube.main'" -O2 CountingCube.lhs
[2 of 2] Compiling CountingCube     ( CountingCube.lhs, CountingCube.o )
Linking CountingCube ...
$ time ./CountingCube > solutions.txt 
Possibilities (explored, kept) at each stage:
  (4,4)
  (256,130)
  (9360,2334)
  (168048,16625)
  (1596000,26403)
  (2534688,4080)
  (587520,240)
Total possibilities explored: 4895876
Total possibilities kept: 49816

real	0m0.070s
user	0m0.061s
sys	0m0.007s
~~~

That really was a valuable optimisation! Instead of trying nearly 200,000,000 moves, this version explores only 4,895,876 - it is over 40 times more efficient. And the solutions are now all found in under a tenth of a second. I am embarrassed that I didn't think of this sooner.
