module Game.CubicalDomset.Notation
       ( PlayerPosition(..)
       , PiecePosition(..)
       , GlobalPosition(..)
       , toGlobalPosition
       , toPlayerPosition
       , fromPlayerPosition
       , fromGlobalPosition)
       where

import qualified Data.Set as Set
import Data.List (elemIndex)

-- Piece positions.

data PiecePosition = V | W | X | Y
                   | One | Two | Three | Four
                   deriving (Eq, Enum, Ord)

-- Player position datatype : defined as a set of piece positions

newtype PlayerPosition = PlayerPosition
                         { getPlayerPosition :: Set.Set PiecePosition
                         } deriving (Eq, Ord, Read)

-- Convert to and from player positions and pairs of piece positions.

toPlayerPosition :: PiecePosition -> PiecePosition -> PlayerPosition
toPlayerPosition x y = PlayerPosition $ Set.fromList [x,y]
{-# INLINE toPlayerPosition #-}

fromPlayerPosition :: PlayerPosition
                   -> (PiecePosition, PiecePosition)
fromPlayerPosition s = let [x,y] = Set.toList $ getPlayerPosition s
                       in  (x,y)
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

showListPiecePosition = ["V","W","X","Y","1","2","3","4"]

{-# INLINE showListPiecePosition #-}

instance Show PiecePosition where
  show = (showListPiecePosition !!) . fromEnum

-- Read instance for PiecePosition.

instance Read PiecePosition where
  readsPrec _ r = do (s,rem) <- lex r
                     let Just k = elemIndex s showListPiecePosition
                     return ((toEnum k),rem)

instance Show PlayerPosition where
  show (PlayerPosition x) = show (Set.toList x)

instance Show GlobalPosition where
  show = show . getGlobalPosition 
