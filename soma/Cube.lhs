> module Cube where

> import Data.Bits
> import Data.Function (on)
> import Data.List (intercalate, nubBy, sortBy, transpose, unfoldr)
> import Data.Monoid
> import Data.Ord (comparing)
> import Data.Word

The shapes remain the same. The last one has been slightly renamed to avoid conflict with the axis of the same name.

> data Shape = L | S | T | R | P | Q | Y'
>   deriving (Enum, Show)

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

> newtype Form = Form [[[Char]]]
> unform (Form x) = x

> instance Eq Form where
>   (Form x) == (Form y) = x == y

> instance Ord Form where
>   Form x `compare` Form y = x `compare` y

> withForm f = Form . f . unform

The order in which rows of a form are displayed has been reversed, so that Y increases up the screen. With X increasing to the right, this means that each layer of the cube is displayed as if viewed from above (looking towards -Z) in a right-handed coordinate system, so it fits Blender naturally.

> instance Show Form where
>   show (Form f) = unlines [showSlice s | s <- reverse $ transpose f]
>    where
>     showSlice s = intercalate "  " [ showRow r | r <- s ]
>     showRow r = "|" ++ r ++ "|"

> instance Monoid Form where
>   mempty = Form $ (replicate 3 . replicate 3 . replicate 3) ' '
>   mappend (Form x) (Form y) = Form $ (zipWith . zipWith . zipWith)
>                                      superpose x y
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

To be able to produce geometrical descriptions of the solutions, it is useful to record not only which shapes occupy which cell of the cube, but also how the shapes' forms are composed. As mentioned before, each form can be constructed from a translation, followed by a rotation about the X axis and then a rotation about either the Y or Z axes.

These transformations could all be represented as matrices, but they're much easier to decipher if given a higher-level representation. This sequence of three transformations is referred to as a recipe.

> data Axis = X | Y | Z deriving (Enum, Show)
>
> newtype Translation = Translation (Int, Int, Int) deriving Show
>
> type QuarterTurns = Int
> data Rotation = Rotation Axis QuarterTurns deriving Show
>
> type Recipe = (Rotation, Rotation, Translation)

Having given names to these transformations, they can now be listed in terms of those names instead of relying directly on function composition.

> majors = [Rotation Y n | n <- [0..3]] ++
>          [Rotation Z n | n <- [1, 3]]
>
> minors = [Rotation X n | n <- [0..3]]
>
> shifts shape = [t | t <- [Translation (x, y, z) |
>                           x <- [0..2],
>                           y <- [0..2],
>                           z <- [0..2]], inBounds t]
>  where
>   inBounds t = size (applyT t $ df) == size df
>   df = defaultForm shape

Translations can now be in either direction; this might prove useful later.

> shiftr def [a, b, _] = [def, a, b]
> shiftl def [_, b, c] = [b, c, def]

> withx op = (map . map) (op ' ')
> withy op = map (op "   ")
> withz op = op (replicate 3 "   ")
> tx  = withx shiftr
> tx' = withx shiftl
> ty  = withy shiftr
> ty' = withy shiftl
> tz  = withz shiftr
> tz' = withz shiftl

> cw = transpose . reverse
> ccw = reverse . transpose

Transformations being defined in terms of Recipes instead of raw functions, a little glue is needed to describe how a recipe is applied to a Form.

Note also that the rotation about X has been changed from clockwise to counter-clockwise. These rotations now match Blender's default settings, which will avoid complication further down the line.

> times 0 f = id
> times n f = f . times (n-1) f
>
> applyR (Rotation a n) = withForm (times n (r a))
>  where
>   r X = cw
>   r Y = transpose . map ccw . transpose
>   r Z = map cw
>
> applyT (Translation (x, y, z)) = withForm (times x tx . times y ty . times z tz)
>
> applyRecipe (maj, min, shift) = applyR maj . applyR min . applyT shift

The special case for the forms of L have been described a little more neatly.

> allForms :: Shape -> [(Recipe, Form)]
> allForms shape = nubBy ((==) `on` snd) $ [(recipe, applyRecipe recipe df) |
>                                           recipe <- recipes shape]
>  where
>   df = defaultForm shape
>   recipes L = [(Rotation Y 0, Rotation X 0, t) |
>                t <- [s | s@(Translation (a, b, c)) <- shifts L, c /= 2]]
>   recipes shape =  [(maj, min, shift) |
>                     maj <- majors,
>                     min <- minors,
>                     shift <- shifts shape]

The list of possible forms for each shape can now carry the recipe for constructing each form, as well as the resulting bit pattern.

> combos :: [(Shape, [(Recipe, Word32)])]
> combos = sortBy (comparing (length . snd))
>                 [(shape, [(r, asBits f) | (r, f) <- allForms shape]) |
>                  shape <- enumFrom L]

> newtype Solution = Solution [(Shape, Recipe, Word32)] deriving (Show)

> solutions :: [Solution]
> solutions = [s | (s, _) <- foldl addForm [(Solution [], 0)] combos] where
>   addForm partialSolutions (shape, formData) = [
>       (Solution ((shape, recipe, bitmap):s), (pb .|. bitmap)) |
>       (Solution s, pb) <- partialSolutions,
>       (recipe, bitmap) <- formData,
>       pb .&. bitmap == 0
>       ]

> expandSolution (Solution s) = foldl1 mappend [fromBits shape bitmap |
>                                               (shape, _, bitmap) <- s]

Finally, this makes it possible to explain what sequence of rotations and transformations was used to place each shape in position in a solution.

> explainSolution (Solution s) = unlines [show (shape, recipe) |
>                                         (shape, recipe, _) <- s]

> main = putStrLn $ intercalate "\n" [show (expandSolution s) | s <- solutions]
