> module Word128 where
> import Data.Bits
> import Data.Word

> newtype WordPair a = WordPair (a, a) deriving (Eq, Show)

> instance (Num a, Bits a) => Num (WordPair a)
>  where
>   fromInteger = fromInteger' undefined
>    where fromInteger' :: (Bits a, Num a) => a -> Integer -> WordPair a
>          fromInteger' x n = WordPair (fromInteger (n `shiftR` bitSize x),
>                                       fromInteger n)

> instance Ord a => Ord (WordPair a)
>  where (WordPair x) `compare` (WordPair y) = x `compare` y

> pieceWise op (WordPair (h, l)) (WordPair (h', l')) = WordPair (h `op` h',
>                                                                l `op` l')

> instance Bits a => Bits (WordPair a) where
>   (.&.) = pieceWise (.&.)
>   (.|.) = pieceWise (.|.)
>   testBit (WordPair (h, l)) n = if n < bitSize l
>                                 then testBit l n
>                                 else testBit h (n - bitSize l)
>   setBit (WordPair (h, l)) n = if n < bitSize l
>                                then WordPair (h, setBit l n)
>                                else WordPair (setBit h (n - bitSize l), l)
>   bitSize (WordPair (x, y)) = bitSize x + bitSize y 

> showBinary :: Bits a => a -> String
> showBinary x = [if testBit x n then '1' else '0' | n <- [w-1,w-2..0]]
>  where w = bitSize x

> type Word128 = WordPair Word64
