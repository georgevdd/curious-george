> module Cube where

> import Data.Bits
> import Data.Function (on)
> import Data.List (intercalate, nubBy, sortBy, transpose, unfoldr)
> import qualified Data.Map as Map
> import Data.Monoid
> import Data.Ord (comparing)
> import Data.Vect hiding (pointwise, transpose)
> import Data.Word

> data Shape = L | S | T | R | P | Q | Y'
>   deriving (Enum, Eq, Ord, Show)

> plan L = "XXX" ++
>          "X  "

> plan S = " XX" ++
>          "XX "

> plan T = "XXX" ++
>          " X "

> plan R = "XX " ++
>          "X  "

> plan P = "X  " ++
>          "X  " ++
>          "   " ++
>          "XX "

> plan Q = " X " ++
>          " X " ++
>          "   " ++
>          "XX "

> plan Y' = "XX " ++
>           "X  " ++
>           "   " ++
>           "X  "

> newtype Form = Form { unform :: [[[Char]]] }

> instance Eq Form where
>   (Form x) == (Form y) = x == y

> instance Ord Form where
>   Form x `compare` Form y = x `compare` y

> withForm f = Form . f . unform

> instance Show Form where
>   show (Form f) = unlines [showSlice s | s <- reverse $ transpose f]
>    where
>     showSlice s = intercalate "  " [ showRow r | r <- s ]
>     showRow r = "|" ++ r ++ "|"

> pointwise :: (Char -> Char -> Char) -> Form -> Form -> Form
> pointwise f (Form x) (Form y) = Form $ (zipWith . zipWith . zipWith) f x y

> instance Monoid Form where
>   mempty = Form $ (replicate 3 . replicate 3 . replicate 3) ' '
>   mappend = pointwise superpose
>    where
>     superpose ' ' ' ' = ' '
>     superpose  a  ' ' =  a
>     superpose ' '  b  =  b
>     superpose  _   _  = '#'  -- This should never happen!

> inThrees = takeWhile (not . null) . unfoldr (Just . splitAt 3)

> flatten = concat . concat . unform

> unflatten = inThrees . inThrees

> asBits :: Form -> Word32
> asBits f = sum [bit n | (n, x) <- zip [0..] (flatten f), x /= ' ']

> fromBits shape bs = Form $ unflatten [formChar (testBit bs n) | n <- [0..26]]
>  where
>   formChar False = ' '
>   formChar True  = head (show shape)

> size f = length [x | x <- flatten f, x /= ' ']

> defaultForm :: Shape -> Form
> defaultForm shape = Form $ unflatten fullPlan
>  where
>   fullPlan = [formChar c | c <- plan shape] ++
>              replicate (27 - length (plan shape)) ' '
>   formChar ' ' = ' '
>   formChar  _  = head (show shape)

> data Axis = X | Y | Z deriving (Enum, Eq, Ord, Show)
>
> type Recipe = Proj4

> withOffset :: Vec3 -> Proj4 -> Proj4
> withOffset o m = translation o .*. m .*. translation (neg o)

The rotation transformations should always map integers coordinates to integer coordinates, but small errors in the underlying trigonometry emerge. This functions turns a rotation matrix into a corresponding projection matrix containing only integral values, and then transforms it to rotate about the centre of the cube.

> exactRotation = withOffset (zero &- Vec3 1.5 1.5 1.5) .
>                 linear . mapVec (fromIntegral . round)

> majors = map exactRotation ([rotMatrixY (n * pi/2) | n <- [0..3]] ++
>                             [rotMatrixZ (n * pi/2) | n <- [1, 3]])
>
> minors = map exactRotation [rotMatrixX (n * pi/2) | n <- [0..3]]
>
> shifts shape = [t | t <- [translation (Vec3 x y z) |
>                           x <- [0..2],
>                           y <- [0..2],
>                           z <- [0..2]], inBounds t]
>  where
>   inBounds t = size (applyRecipe t $ df) == size df
>   df = defaultForm shape

This implementation of applyRecipe avoids inverting the recipe's transformation matrix. Instead, it builds a table describing which coordinates ended up holding which source cells, then samples the resulting Form by querying that table. It's not pretty, but it works. The offset by 0.5 means rotations map cell centres (indexed by [0..2]) to cell centres, instead of mapping cell corners (indexed by [0..3]) to cell corners.

> applyRecipe r (Form layers) = Form [[[Map.findWithDefault ' ' (x, y, z) cells |
>                                       x <- [0..2]] |
>                                      y <- [0..2]] |
>                                     z <- [0..2]]
>  where cells = Map.fromList [((round x, round y, round z), c) |
>                              ((Vec4 x y z _), c) <- inverse]
>        inverse = [(Vec4 x y z 1 .* r', cell) |
>                   (z, layer) <- zip [0..2] layers,
>                   (y, row) <- zip [0..2] layer,
>                   (x, cell) <- zip [0..2] row]
>        r' = fromProjective $ withOffset (Vec3 0.5 0.5 0.5) r

> data Symmetry = RotationOnly | RotationAndReflection

> allForms :: Shape -> Symmetry -> [(Recipe, Form)]
> allForms shape symmetry = nubBy ((==) `on` snd) $
>                           [(recipe, applyRecipe recipe df) |
>                            recipe <- recipes shape]
>  where
>   df = defaultForm shape
>   recipes L = [s | s <- shifts L, symmetry `allows` s]
>   recipes shape = allRecipes shape

To eliminate reflection symmetries, cases where L is translated by 2 along Z are eliminated. (They'd all be reflections of cases where L is translated by 0 along Z.) The translation component of the transform is in the fourth row of the matrix. The Z component of the translation is the third component of that.

>   RotationOnly          `allows` _ = True
>   RotationAndReflection `allows` s = (_3 $ _4 $ fromProjective s) /= 2

>   allRecipes shape =  [shift .*. min .*. maj |
>                        maj <- majors,
>                        min <- minors,
>                        shift <- shifts shape]

> combos :: Symmetry -> [(Shape, [(Recipe, Word32)])]
> combos symmetry = sortBy (comparing (length . snd))
>                   [(shape, [(r, asBits f) |
>                             (r, f) <- allForms shape symmetry]) |
>                    shape <- enumFrom L]

> newtype Solution = Solution [(Shape, Recipe, Word32)] deriving (Show)

> solutions' :: Symmetry -> [Solution]
> solutions' symmetry = [s | (s, _) <- foldl addForm [(Solution [], 0)] $
>                                      combos symmetry] where
>   addForm partialSolutions (shape, formData) = [
>       (Solution ((shape, recipe, bitmap):s), (pb .|. bitmap)) |
>       (Solution s, pb) <- partialSolutions,
>       (recipe, bitmap) <- formData,
>       pb .&. bitmap == 0
>       ]

By default, only solutions distinct up to rotation and reflection are returned.

> solutions = solutions' RotationAndReflection

> expandSolution (Solution s) = foldl1 mappend [fromBits shape bitmap |
>                                               (shape, _, bitmap) <- s]

> explainSolution (Solution s) = unlines [show (shape, recipe) |
>                                         (shape, recipe, _) <- s]

> main = putStrLn $ intercalate "\n" [show (expandSolution s) | s <- solutions]
