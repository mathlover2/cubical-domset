module Game.CubicalDomset.Notation
       ( toPlayerPosition
       , fromPlayerPosition
       , PlayerPosition
       , PiecePosition(..)
       , GlobalPosition(..)
       , toGlobalPosition
       , fromGlobalPosition)
       where

import Data.List (elemIndex)
import Text.Read

-- Piece positions.

data PiecePosition = V | W | X | Y
                   | One | Two | Three | Four
                   deriving (Eq, Enum, Ord)

-- Player position datatype : defined as unordered pair of piece positions

data PlayerPosition = PlayerPosition !PiecePosition !PiecePosition deriving Ord

-- Convert to and from player positions and pairs of piece positions.

toPlayerPosition :: PiecePosition -> PiecePosition -> PlayerPosition
toPlayerPosition x y = if x > y then PlayerPosition y x else PlayerPosition x y
{-# INLINE toPlayerPosition #-}

fromPlayerPosition :: PlayerPosition
                   -> (PiecePosition, PiecePosition)
fromPlayerPosition (PlayerPosition x y) = (x,y)
{-# INLINE fromPlayerPosition #-}

-- GlobalPosition : an ordered pair of player positions. 

newtype GlobalPosition = GlobalPosition
                         { getGlobalPosition :: (PlayerPosition, PlayerPosition)
                         } deriving (Eq, Ord)

-- Convert to and from global positions and pairs of piece positions.

toGlobalPosition :: PlayerPosition -> PlayerPosition -> GlobalPosition
toGlobalPosition x y = GlobalPosition (x,y)
{-# INLINE toGlobalPosition #-}

fromGlobalPosition :: GlobalPosition -> (PlayerPosition, PlayerPosition)
fromGlobalPosition = getGlobalPosition
{-# INLINE fromGlobalPosition #-}

-- Show instance for PiecePosition.

showListPiecePosition :: Int -> String
showListPiecePosition 0 = "V"
showListPiecePosition 1 = "W"
showListPiecePosition 2 = "X"
showListPiecePosition 3 = "Y"
showListPiecePosition 4 = "1"
showListPiecePosition 5 = "2"
showListPiecePosition 6 = "3"
showListPiecePosition 7 = "4"

{-# INLINE showListPiecePosition #-}

instance Show PiecePosition where
  show = showListPiecePosition . fromEnum

-- Read instance for PiecePosition.

instance Read PiecePosition where
  readsPrec _ r = do (s,rem) <- lex r
                     let Just k = elemIndex s l
                     return ((toEnum k),rem)
    where l = map (:[]) "VWXY1234"

instance Show GlobalPosition where
  show = show . getGlobalPosition 

-- Eq, Read, and Show instances for PlayerPosition

instance Eq PlayerPosition where
  (PlayerPosition x y) == (PlayerPosition x' y')
    = (x == x' && y == y') || (x == y') && (y == x')

instance Show PlayerPosition where
  show = show . fromPlayerPosition

instance Read PlayerPosition where
  readsPrec _ r = do ("(",s) <- lex r
                     (p1s,s) <- lex s
                     (",",s) <- lex s
                     (p2s,s) <- lex s
                     (")",t) <- lex s
                     let p1 = read p1s
                         p2 = read p2s
                     return (PlayerPosition p1 p2,t)
                        
