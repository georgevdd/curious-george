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

> newtype Cube = Cube [[[Char]]]
> emptyCube = Cube $ (replicate 3 . replicate 3 . replicate 3) ' '

> type Snake = String
> snake :: Snake
> snake = "bwb/w/b/wb/w/bw/b/w/bw/bw/b/w/b/wb/wb/wb/wb"

> fill :: Cube -> Position -> Cube
> fill (Cube c) (x, y, z) = Cube (update c z $ update (c!!z) y $ update (c!!z!!y) x 'x')
>   where update xs i x = take i xs ++ [x] ++ drop (i+1) xs
> isFilledAt :: Cube -> Position -> Bool
> isFilledAt (Cube c) (x, y, z) = c!!z!!y!!x /= ' '

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
