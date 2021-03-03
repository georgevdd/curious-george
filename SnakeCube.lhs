> import Data.Bits (setBit, testBit)

> data Axis = X | Y | Z deriving (Eq, Show)
> data Sign = Positive | Negative deriving (Eq, Show)
> data Direction = Direction Axis Sign
> type Position = (Int, Int, Int)

> instance Show Direction where
>   show (Direction X Positive) = ">"
>   show (Direction X Negative) = "<"
>   show (Direction Y Positive) = "^"
>   show (Direction Y Negative) = "v"
>   show (Direction Z Positive) = "."
>   show (Direction Z Negative) = "o"

> nextDirections :: Direction -> [Direction]
> nextDirections (Direction a _) = [Direction a' d |
>                                   a' <- [X, Y, Z],
>                                   a' /= a,
>                                   d <- [Positive, Negative]]

> nextPosition :: Position -> Direction -> Position
> nextPosition (x, y, z) (Direction a d) =
>   case a
>   of X -> (x+delta, y, z)
>      Y -> (x, y+delta, z)
>      Z -> (x, y, z+delta)
>   where delta = if d == Positive then 1 else -1

> outOfBounds :: Position -> Bool
> outOfBounds (x, y, z) = or [i < 0 || i > 2 | i <- [x, y, z]]

> type Snake = String
> snake :: Snake
> snake = "bwb/w/b/wb/w/bw/b/w/bw/bw/b/w/b/wb/wb/wb/wb"

> type Cube = Int
> emptyCube = 0

> bitIndex :: Position -> Int
> bitIndex (x, y, z) = x*9 + y*3 + z
> fill :: Cube -> Position -> Cube
> fill cube pos = setBit cube (bitIndex pos)
> isFilledAt :: Cube -> Position -> Bool
> isFilledAt cube pos = testBit cube (bitIndex pos)

> type PartialSolution = [Direction]
> type Solution = PartialSolution

> solutions' :: Cube
>           -> Position
>           -> Direction
>           -> Snake
>           -> PartialSolution
>           -> [Solution]
> solutions' _ _ dir [] soFar = [dir:soFar]
> solutions' cube prevPos dir ('/':snake') soFar =
>   concat [solutions' cube prevPos dir' snake' (dir:soFar) |
>           dir' <- nextDirections dir]
> solutions' cube prevPos dir (c:snake') soFar =
>   let pos = nextPosition prevPos dir in
>   if outOfBounds pos || cube `isFilledAt` pos then []
>   else solutions' (fill cube pos) pos dir snake' soFar

> solutions snake = map reverse $ solutions' emptyCube
>                                            (-1, 0, 0)
>                                            (Direction X Positive)
>                                            snake
>                                            []

> main = mapM_ print $ map (unwords . map show) $ solutions snake
