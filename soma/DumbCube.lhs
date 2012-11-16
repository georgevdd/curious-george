In <a href="http://future-indicative.blogspot.com/2012/10/mike-has-cube-like-this.html">a previous post</a> I investigated the number of ways of solving a <a href="http://wikipedia.org/wiki/Soma_cube">Soma cube</a> by writing a Haskell program to find out for me. I was pleased to find that an optimised version of the program took about 1.2 seconds to find 240 solutions.

In that program, I represented each partial solution and each position of a shape as a 27-bit number, allowing simple bitwise operations to combine forms and filter out the invalid ones. Was that a premature optimisation? It would definitely have been faster in terms of programming time not to write all the conversion to bit patterns, and to operate directly on the representation of forms as lists of characters. But would the resulting program have run as fast?

A different version of just a few functions will show whether the extra work was necessary.

> module DumbCube where
> import Cube hiding (main)
> import Data.Monoid (mappend)
> import Data.List (intercalate)

For each shape, all the forms are precalculated, but not the bit representations that `Cube.combos` computes.

> dumbCombos = [allForms shape | shape <- enumFrom L]

Likewise, the fold to enumerate solutions combines form representations directly, instead of working on any bit representations.

> dumbSolutions = foldr1 addForm dumbCombos where
>   addForm partialSolution form = [cf |
>                                   cf <- [(pf `mappend` f) |
>                                          f <- form,
>                                          pf <- partialSolution],
>                                   not ('#' `elem` flatten cf)]

Finally, as before, print out all the solutions, separated by newlines.

> main = putStrLn $ intercalate "\n" [show s | s <- dumbSolutions]

How fast is the result now?

~~~
$ ghc --make -main-is DumbCube -O2 DumbCube.lhs
[1 of 2] Compiling Cube             ( Cube.lhs, Cube.o )
[2 of 2] Compiling DumbCube         ( DumbCube.lhs, DumbCube.o )
Linking DumbCube ...
$ time ./DumbCube > solutions.txt 

real	4m0.143s
user	3m58.932s
sys	0m1.072s
~~~

This slightly simpler program produces the same output but takes about 240 seconds - almost exactly one second per solution, on average. That also means it's almost exactly 200 times slower than the original program with its bitwise representation. That conversion to patterns of 27 bits obviously was worthwhile!
