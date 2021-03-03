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

> newtype Form = Form [[[Char]]]
> emptyForm = Form $ (replicate 3 . replicate 3 . replicate 3) ' '

> type Snake = String
> snake :: Snake
> snake = "bwb/w/b/wb/w/bw/b/w/bw/bw/b/w/b/wb/wb/wb/wb"

> fill :: Form -> Position -> Form
> fill (Form f) (x, y, z) = Form (update f z $ update (f!!z) y $ update (f!!z!!y) x 'x')
>   where update xs i x = take i xs ++ [x] ++ drop (i+1) xs
> filled :: Form -> Position -> Bool
> filled (Form f) (x, y, z) = f!!z!!y!!x /= ' '

> type PartialSolution = [Direction]
> type Solution = PartialSolution

> solutions' :: Form
>           -> Position
>           -> Direction
>           -> Snake
>           -> PartialSolution
>           -> [Solution]
> solutions' _ _ dir [] soFar = [dir:soFar]
> solutions' f prevPos dir ('/':snake') soFar =
>   concat [solutions' f prevPos dir' snake' (dir:soFar) | dir' <- nextDirections dir]
> solutions' f prevPos dir (c:snake') soFar =
>   let pos = nextPosition prevPos dir in
>   if outOfBounds pos || filled f pos then []
>   else solutions' (fill f pos) pos dir snake' soFar

> solutions snake = map reverse $ solutions' emptyForm (-1, 0, 0) (Direction X Positive) snake []

> main = mapM_ print $ map (unwords . map show) $ solutions snake