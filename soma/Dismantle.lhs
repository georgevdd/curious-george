> module Dismantle where

> import Data.List
> import Data.Monoid (mappend, mconcat)

> import Cube
> import Mesh
> import Data.Vect (Vec3(..), translation)

> dismantleOrder :: Solution -> [Shape]
> dismantleOrder (Solution s) = unfoldr pickShape
>                                       (forms, mconcat $ map snd forms)
>  where forms = [(shape, applyRecipe recipe $ defaultForm shape) |
>                 (shape, recipe, _) <- s]

> pickShape ::               ([(Shape, Form)], Form)
>           -> Maybe (Shape, ([(Shape, Form)], Form))
> pickShape ([], _) = Nothing
> pickShape ((shape, form):sfs, partialSolution) =
>     if form `isFreeIn` partialSolution
>     then Just (shape, (sfs, partialSolution `minus` form))
>     else fmap (\(shape', (sfs',              partialSolution')) ->
>                 (shape', ((shape,form):sfs', partialSolution'))) $
>               pickShape (sfs, partialSolution)

> isFreeIn :: Form -> Form -> Bool
> form `isFreeIn` partialSolution = isValid $ (partialSolution `minus` form)
>                                             `mappend`
>                                             (applyRecipe upOne form)
>  where
>   upOne = translation (Vec3 0 0 1)
>   isValid = not . ('#' `elem`) . flatten

> minus :: Form -> Form -> Form
> minus = pointwise clear
>  where
>   clear  a ' ' =  a
>   clear  _  b  = ' '
