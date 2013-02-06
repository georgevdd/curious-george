> module Cube where

> import Data.Bits
> import Data.Function (on)
> import Data.List (intercalate, nubBy, sortBy, transpose, unfoldr)
> import Data.Monoid
> import Data.Ord (comparing)
> import Data.Word

'Shape' now derives 'Eq' and 'Ord'.

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

'Axis' also now derives 'Eq' and 'Ord'.

> data Axis = X | Y | Z deriving (Enum, Eq, Ord, Show)
>
> newtype Translation = Translation (Int, Int, Int) deriving Show
>
> type QuarterTurns = Int
> data Rotation = Rotation Axis QuarterTurns deriving Show
>
> type Recipe = (Rotation, Rotation, Translation)

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

This type describes which kinds of symmetry of the cube should be considered
identical when computing solutions. The number of solutions modulo reflection
and rotation is half the number of solutions modulo rotation only. For simply
counting solutions, the first (smaller) number is appropriate, but for more
involved computations such as considering which solutions' faces are disjoint
it is necessary to consider mirror images separately.

> data Symmetry = RotationOnly | RotationAndReflection

The list of all forms of a shape now needs to depend on what symmetries are
allowed.

> allForms :: Shape -> Symmetry -> [(Recipe, Form)]
> allForms shape symmetry = nubBy ((==) `on` snd) $
>                           [(recipe, applyRecipe recipe df) |
>                            recipe <- recipes shape]
>  where
>   df = defaultForm shape
>   recipes L = [r | r@(Rotation _ y,
>                       Rotation _ x,
>                       Translation (a, b, c)) <- allRecipes L,
>                y == 0,
>                x == 0,
>                symmetry `allows` c]
>   recipes shape = allRecipes shape

>   RotationOnly          `allows` c = True
>   RotationAndReflection `allows` c = (c /= 2)

>   allRecipes shape =  [(maj, min, shift) |
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
