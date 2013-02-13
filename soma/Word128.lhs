> {-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(a,b,c) 0
#endif

#if MIN_VERSION_base(4,5,0) || (__GLASGOW_HASKELL__ >= 704)
#define HAVE_popCount
#endif

> module Word128 where
> import Data.Bits
> import Data.Word

#ifndef HAVE_popCount
> import Misc (popCount)
#endif

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
>   complement (WordPair (h, l)) = WordPair (complement h, complement l)
>   WordPair (h, l) `shiftL` n = WordPair ((h `shiftL` n) .|.
>                                          (l `shiftR` (bitSize l - n)),
>                                          (l `shiftL` n))
>   testBit (WordPair (h, l)) n = if n < bitSize l
>                                 then testBit l n
>                                 else testBit h (n - bitSize l)
>   setBit (WordPair (h, l)) n = if n < bitSize l
>                                then WordPair (h, setBit l n)
>                                else WordPair (setBit h (n - bitSize l), l)
>   bitSize (WordPair (h, l)) = bitSize h + bitSize l
#ifdef HAVE_popCount
>   popCount (WordPair (h, l)) = popCount h + popCount l
#endif

> type Word128 = WordPair Word64
