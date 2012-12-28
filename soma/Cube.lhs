> module Cube where

> import Data.Bits
> import Data.Function (on)
> import Data.List (intercalate, nubBy, sortBy, transpose, unfoldr)
> import Data.Monoid
> import Data.Ord (comparing)
> import Data.Word

There are seven shapes. The names here are intended to suggest their forms where possible. The P and Q shapes don't really look like any letter. They are mirror images of each other, though.

> data Shape = L | S | T | R | P | Q | Y'
>   deriving (Enum, Show)

Each shape has a plan, which describes the cells of the cube that it occupies if placed in the left back corner of the bottom layer of the cube. The plan describes each row of each layer of the cube in turn, up to the point where all the remaining rows are empty. So the L plan can stop after only two rows, but the Y plan needs to go as far as the first row of the second layer.

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

The terminology 'form' is introduced to mean an assignment of shapes to positions in the cube. A form could be represented in all sorts of ways - 27 bits, or a string of 27 characters, or an array of booleans, or something else. A three-dimensional list of characters is slightly ugly, in that it is quite possible to construct forms that have the wrong dimensions or meaningless contents. But it has the advantage of allowing lots of built-in list manipulation and display functions to be brought to bear.

> newtype Form = Form [[[Char]]]
> unform (Form x) = x

It's useful to be able to compare and sort forms. Again, the choice of representation allows different encodings of the same form to seem unequal when they shouldn't, but that isn't a problem in practice.

> instance Eq Form where
>   (Form x) == (Form y) = x == y

> instance Ord Form where
>   Form x `compare` Form y = x `compare` y

> withForm f = Form . f . unform

An easily readable representation of a form is helpful for listing the solutions (as well as for playing with individual shapes' forms to show that things are working properly). Transposing the layers of rows allows the successive layers to be printed next to one another instead of above one another to make better use of screen space - an example of the list representation making things easier.

> instance Show Form where
>   show (Form f) = unlines [showSlice s | s <- transpose f]
>    where
>     showSlice s = intercalate "  " [ showRow r | r <- s ]
>     showRow r = "|" ++ r ++ "|"

There is an empty form (corresponding to the partial solution where no shapes have been placed yet) and it's easy to see how to combine a form with another form and that (if the combination is valid) this will be a symmetric operation, so these forms naturally produce a Monoid.

> instance Monoid Form where
>   mempty = Form $ (replicate 3 . replicate 3 . replicate 3) ' '
>   mappend (Form x) (Form y) = Form $ (zipWith . zipWith . zipWith)
>                                      superpose x y
>    where
>     superpose ' ' ' ' = ' '
>     superpose  a  ' ' =  a
>     superpose ' '  b  =  b
>     superpose  _   _  = '#'  -- This should never happen!

To compute the bit pattern for a form, string the characters for each of its rows together and then convert each character in turn into a single bit.

> inThrees = takeWhile (not . null) . unfoldr (Just . splitAt 3)

> flatten = concat . concat . unform
> unflatten = inThrees . inThrees

> asBits :: Form -> Word32
> asBits f = sum [bit n | (n, x) <- zip [0..] (flatten f), x /= ' ']

> fromBits shape bs = Form $ unflatten $ map (formChar . testBit bs) [0..26]
>  where
>   formChar False = ' '
>   formChar True  = head (show shape)

It'll be useful to know how many cells of the cube are occupied by a given form.

> size f = length [x | x <- flatten f, x /= ' ']

As mentioned before, the plan of each shape describes the cells it occupies when pushed into a particular corner of the cube. Here is a way to take this plan (which doesn't even describe the entire cube!) and turn it into a complete form suitable for combining with other forms.

> defaultForm :: Shape -> Form
> defaultForm shape = Form $ unflatten fullPlan
>  where
>   fullPlan = [formChar c | c <- plan shape] ++
>              replicate (27 - length (plan shape)) ' '
>   formChar ' ' = ' '
>   formChar  _  = head (show shape)


> data Axis = X | Y | Z deriving (Enum, Show)

> type Cells = Int
> newtype Translation = Translation (Cells, Cells, Cells) deriving Show

> type QuarterTurns = Int
> data Rotation = Rotation Axis QuarterTurns deriving Show

> type Recipe = (Rotation, Rotation, Translation)

These translation and rotation functions are a basis for enumerating all the forms that a shape can produce. As discussed earlier, each shape can be rotated to have its base against one of six faces of the cube ...

> majors = [Rotation Y n | n <- [0..3]] ++
>          [Rotation Z n | n <- [1, 3]]

... and then rotated in the plane of that face in one of four ways ...

> minors = [Rotation X n | n <- [0..3]]

... and then translated by up to two cells in each of three directions, but only so long as the shape still lies within the bounds of the cube.

> shifts shape = [t | t <- [Translation (x, y, z) |
>                           x <- [0..2],
>                           y <- [0..2],
>                           z <- [0..2]], inBounds t]
>  where
>   inBounds t = size (applyT t $ df) == size df
>   df = defaultForm shape


Translation functions take a form for a shape, and give a form for the same shape but shifted by one place along one of the three axes. Because the default forms of the shapes are always in one particular corner, these translation functions only need to cover shifts away from that one corner in order to be able to describe all the possible positions into which the shape can be translated.

These functions implicitly fix the coordinate system of the form representation: a list of layers, each of which is a list of rows, each of which is a list of cells. This means, for example, that manipulating the outermost list of the structure corresponds to dealing with the cube layer-wise.

Each of these functions inserts empty space on one face of the cube and chops one slice of cells off the opposite face of the cube. Again, the list representation makes them easy to define.

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

The rotation functions are slightly more complicated. For convenience, they are based on a 90-degree rotation being equivalent to a transposition followed by a reflection.

> cw = transpose . reverse
> ccw = reverse . transpose

Rotation about the X axis leaves each row unchanged, so it's just the rotation applied to the outer two layers of lists.

> rx = ccw

To rotate about the Y axis, first transpose Z and Y to bring the Z and X axes next to each other, then apply the rotation to those Z and X axes, then transpose Y and Z to restore the coordinate system. Note that the direction of the applied rotation is reversed because transposition reverses the chirality of the representation.

> ry = (transpose . map ccw . transpose)

Finally, to rotate about Z, apply the desired rotation to each layer in turn.

> rz = (map cw)


> times 0 f = id
> times n f = f . times (n-1) f

> applyR (Rotation a n) = withForm (times n (r a))
>  where
>   r X = rx
>   r Y = ry
>   r Z = rz

> applyT (Translation (x, y, z)) = withForm (times x tx . times y ty . times z tz)

> applyRecipe (maj, min, shift) = applyR maj . applyR min . applyT shift

So, all the forms that a shape can have can be listed by applying each combination of major rotation, minor rotation and shift to the default form for the shape. Some of these will be duplicates because of the symmetries of the shape. @nubBy@ is used to eliminate them, which is quite inefficient but this only needs to be computed once for each shape.

As described so far, combining these lists of forms will give 48 times more solutions than are interesting, because every solution that is merely the rotation or reflection of another solution will also be found. An easy way to avoid that is to fix the orientation of one of the shapes - say, the first one. Then, there can't be any solutions that are simply rotations of another solution, because that one piece is always facing the same way. Similarly, omit any translations for it that give any forms which are mirror images. These considerations reduce the forms for the L shape to just four.

> allForms :: Shape -> [(Form, Recipe)]
> allForms shape = nubBy ((==) `on` fst) $ [(applyRecipe t $ df, t) |
>                                           t <- transforms shape]
>  where
>   df = defaultForm shape
>   transforms L = [(Rotation Y 0, Rotation X 0, t) |
>                   t <- [s | s@(Translation (a, b, c)) <- shifts L, c /= 2]]
>   transforms shape =  [(maj, min, shift) |
>                        maj <- majors,
>                        min <- minors,
>                        shift <- shifts shape]

For each shape, the bit representations of all the forms it can have. Keeping this list on its own avoids computing a shape's forms and their bit representations afresh for each new partial solution in which the shape is being considered.

> combos :: [(Shape, [(Word32, Recipe)])]
> combos = sortBy (comparing (length . snd))
>                 [(shape, [(asBits f, r) | (f, r) <- allForms shape]) |
>                  shape <- enumFrom L]

> type PartialSolution = ([Word32], Word32, [Recipe])

> rawSolutions :: [PartialSolution]
> rawSolutions = foldl addForm [([], 0, [])] formBits where
>   formBits = map snd combos
>   addForm partialSolution form = [(b:bs, pb .|. b, r:rs) |
>                                   (b, r) <- form,
>                                   (bs, pb, rs) <- partialSolution,
>                                   pb .&. b == 0]

> solutions = map expandSolution rawSolutions where
>   expandSolution :: ([Word32], a, b) -> Form
>   expandSolution = foldl1 mappend . map (uncurry fromBits) . zip shapes . reverse . xx
>   shapes = map fst combos
>   xx (a, _, _) = a

> explainSolution = unlines . map explainForm . zip shapes . reverse . thd
>  where
>   explainForm :: (Shape, Recipe) -> String
>   explainForm (shape, r) = show shape ++ show r
>   (shapes, formBits) = unzip combos
>   thd (_, _, c) = c

Finally, print out all the solutions, separated by newlines.

> main = putStrLn $ intercalate "\n" [show s | s <- solutions]
