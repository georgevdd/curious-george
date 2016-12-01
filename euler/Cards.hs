module Cards where

import Data.Char (ord)
import Data.List (elemIndex, group, sort, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (Down(..), comparing)
import Lib (equalBy)
import Text.ParserCombinators.ReadP as ReadP

data Rank = Numeric Int | Jack | Queen | King | Ace deriving (Eq, Ord)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Enum)
data Card = Card Rank Suit deriving (Eq, Ord)

instance Enum Rank
 where
  fromEnum (Numeric n) = n
  fromEnum r = 11 + fromJust (elemIndex r [Jack, Queen, King, Ace])
  toEnum n | n >= 2 && n <= 10 = Numeric n
           | n >= 11 && n <= 14 = [Jack, Queen, King, Ace] !! (n - 11)
           | otherwise = error ("Invalid rank " ++ show n)

rank (Card r _) = r
suit (Card _ s) = s

instance Show Rank where
  show (Numeric 10) = "T"
  show (Numeric n) = show n
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

instance Show Suit where
  show Spades = "S"
  show Hearts = "H"
  show Diamonds = "D"
  show Clubs = "C"

instance Show Card where
  show (Card rank suit) = show rank ++ show suit

readRank :: ReadP.ReadP Rank
readRank = do
  c <- ReadP.get
  return $ case c of
    'T' -> (Numeric 10)
    'J' -> Jack
    'Q' -> Queen
    'K' -> King
    'A' -> Ace
    otherwise -> readRankNumeric c
readRankNumeric c | c >= '2' && c <= '9' = Numeric (ord c - ord '0')
                  | otherwise = error ("Unknown rank '" ++ [c] ++ "'")

readSuit :: ReadP.ReadP Suit
readSuit = do
  c <- ReadP.get
  return $ case c of
    'S' -> Spades
    'H' -> Hearts
    'D' -> Diamonds
    'C' -> Clubs
    otherwise -> error ("Unknown suit '" ++ [c] ++ "'")

readCard :: ReadP.ReadP Card
readCard = do
  rank <- readRank
  suit <- readSuit
  return $ Card rank suit

instance Read Rank where
  readsPrec _ = ReadP.readP_to_S readRank

instance Read Suit where
  readsPrec _ = ReadP.readP_to_S readSuit

instance Read Card where
  readsPrec _ = ReadP.readP_to_S readCard


matchRanks :: [Card] -> [[Rank]]
matchRanks = sortBy (comparing $ Down . length) . group . sortBy (comparing Down) . map rank

straight cs = sort [c'' - c' | c'' <- cs'] == [1..4]
 where (c':cs') = sort [fromEnum $ rank c | c <- cs]
flush (c:cs) = all (equalBy suit c) cs      
royal cs = all ((>= Numeric 10) . rank) cs

data PokerScore
  = HighCard
  | Pair
  | TwoPairs
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  | RoyalFlush
 deriving (Show, Eq, Ord)

pokerScore :: [Card] -> (PokerScore, [[Rank]])
pokerScore cs = (scoreType, grouping)
 where
  grouping = matchRanks cs
  scoreType = case [length g | g <- grouping] of
    (4:_) -> FourOfAKind
    [3, 2] -> FullHouse
    (3:_) -> ThreeOfAKind
    (2:2:_) -> TwoPairs
    (2:_) -> Pair
    otherwise -> ungroupedScoreType cs
  ungroupedScoreType cs = case (straight cs, flush cs) of
    (True, True) -> if royal cs then RoyalFlush else StraightFlush
    (True, False) -> Straight
    (False, True) -> Flush
    (False, False) -> HighCard

data Player = Player Int deriving Show

testHands = [parse h | h <- [(
  "5H 5C 6S 7S KD", -- Pair of Fives
  "2C 3S 8S 8D TD", -- Pair of Eights
  Player 2
  ), (
  "5D 8C 9S JS AC", -- Highest card Ace
  "2C 5C 7D 8S QH", -- Highest card Queen
  Player 1
  ), (
  "2D 9C AS AH AC", -- Three Aces
  "3D 6D 7D TD QD", -- Flush with Diamonds
  Player 2
  ), (
  "4D 6S 9H QH QC", -- Pair of Queens, -- Highest card Nine
  "3D 6D 7H QD QS", -- Pair of Queens, -- Highest card Seven
  Player 1
  ), (
  "2H 2D 4C 4D 4S", -- Full House With Three Fours
  "3C 3D 3S 9S 9D", -- Full House with Three Threes
  Player 1
  )]]
 where
  parse (p1, p2, winner) = (parseHand p1, parseHand p2, winner)
 
parseHand s = [read h | h <- words s] :: [Card]
