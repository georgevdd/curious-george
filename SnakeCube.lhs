> import Data.Bits (setBit, testBit)

> type Snake = String
> snake :: Snake
> snake = "bwb/w/b/wb/w/bw/b/w/bw/bw/b/w/b/wb/wb/wb/wb"

This represents the snake that we're trying to fit into a cube. 'b' and
'w' represent black and white blocks, and '/' represents a change of
direction. The only thing that matters for the solution is the location
of the direction changes in this list; the 'b' and 'w' just make it
easier to check that the representation matches the real object.

> type Cube = Int
> emptyCube = 0

This represents the state of the cube into which we're trying to fit
the snake. For a backtracking search, all we really need to record here
is which of the 3x3x3 cells in the cube have already been filled - so
that we don't produce solutions where the snake intersects itself. So
we can use the first 27 bits of an Int.

Now we're ready to define functions for working with the cube:
one to mark a cell as filled and one to see whether it's filled.

> bitIndex :: Position -> Int
> bitIndex (x, y, z) = x*9 + y*3 + z
> fill :: Cube -> Position -> Cube
> fill cube pos = setBit cube (bitIndex pos)
> isFilledAt :: Cube -> Position -> Bool
> isFilledAt cube pos = testBit cube (bitIndex pos)

Our algorithm will work its way through the snake and the cube
simultaneously, marking cells in the cube as used as it goes.
It will need to know where it is in the cube, and in what direction
it's working.

> type Position = (Int, Int, Int)
> data Axis = X | Y | Z deriving (Eq, Show)
> data Sign = Positive | Negative deriving (Eq, Show)
> data Direction = Direction Axis Sign

If the algorithm encounters a change-direction marker in the snake
then it can go in any of the four directions that are perpendicular
to the current direction.

> nextDirections :: Direction -> [Direction]
> nextDirections (Direction a _) = [Direction a' d |
>                                   a' <- [X, Y, Z],
>                                   a' /= a,
>                                   d <- [Positive, Negative]]

Otherwise, it will just move to the next cell, in the direction in
which it's already heading.

> nextPosition :: Position -> Direction -> Position
> nextPosition (x, y, z) (Direction a d) =
>   case a
>   of X -> (x+delta, y, z)
>      Y -> (x, y+delta, z)
>      Z -> (x, y, z+delta)
>   where delta = if d == Positive then 1 else -1

We'll need to know when we've reached the edge of the space.

> outOfBounds :: Position -> Bool
> outOfBounds (x, y, z) = or [i < 0 || i > 2 | i <- [x, y, z]]

Now we have all the parts we need to put together a search algorithm.
We'll build up a list of the directions in which the segments of
the snake need to lie; that's enough information to follow and it's
easier to interpret than, say, a list of the cell coordinates.

> type PartialSolution = [Direction]
> type Solution = PartialSolution

Here's the core of the algorithm: a function that consumes one
character of the snake description, works out what possible next
steps there are, and collects the solutions found by each of
those steps. At each step, it needs to know:

 - which cells of the cube have already been filled;
 - what the last-filled position and direction are;
 - what part of the snake remains to be placed in the cube;
 - what solution has been accumulated so far.

> solutions' :: Cube
>           -> Position
>           -> Direction
>           -> Snake
>           -> PartialSolution
>           -> [Solution]

If there's nothing of the snake left to place, then we've found
one solution. This is the base case for the recursive search.

> solutions' _ _ dir [] soFar = [dir:soFar]

If the next character in the snake indicates a change of direction,
then we have to look for solutions in each of the four possible
next directions. We don't change position or mark any cell as
filled, but we'll need to include the change of direction in any
solutions we find.

> solutions' cube prevPos dir ('/':snake') soFar =
>   concat [solutions' cube prevPos dir' snake' (dir:soFar) |
>           dir' <- nextDirections dir]

Otherwise, we must mark our position as filled - or return
empty-handed if that's impossible - and move to the next cell, in
the same direction as before.

> solutions' cube prevPos dir (c:snake') soFar =
>   let pos = nextPosition prevPos dir in
>   if outOfBounds pos || cube `isFilledAt` pos then []
>   else solutions' (fill cube pos) pos dir snake' soFar

A small wrapper describes the starting conditions for the search.

> solutions snake = map reverse $ solutions' emptyCube
>                                            (-1, 0, 0)
>                                            (Direction X Positive)
>                                            snake
>                                            []

Finally, we will show the directions in a solution as crude arrows.

> instance Show Direction where
>   show (Direction X Positive) = ">"
>   show (Direction X Negative) = "<"
>   show (Direction Y Positive) = "^"
>   show (Direction Y Negative) = "v"
>   show (Direction Z Positive) = "."
>   show (Direction Z Negative) = "o"

> main = mapM_ putStrLn $ map (unwords . map show) $ solutions snake
