{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Game.CubicalDomset.Rules
       where

import Game.CubicalDomset.Notation

import Data.Foldable (foldMap)
import Data.Function (on)
import Data.List ( (\\), nub, inits )
import Control.Monad
import Data.Monoid (All(..))
import Data.Set (fromList, Set, member, unions, toList)
import qualified Data.Set as S
import Data.Tuple (swap)

{-| Basic infrastructure -}

-- Player datatype.

data Player = Player1 | Player2 deriving (Eq, Enum, Ord, Read, Show)

switch :: Player -> Player
switch Player1 = Player2
switch Player2 = Player1

-- Starting position and victory condition tests.

player1_start, player2_start :: PlayerPosition
player1_start = toPlayerPosition V W
player2_start = toPlayerPosition X Y
{-# INLINE player1_start #-}
{-# INLINE player2_start #-}

start :: GlobalPosition
start = toGlobalPosition player1_start player2_start
{-# INLINE start #-}

-- Connections between positions. A position is accessible to another
-- if it is connected by a line on the gameboard.

inner = [V,W,X,Y]
{-# INLINE inner #-}
outer = [One,Two,Three,Four]
{-# INLINE outer #-}

type Link = Set PiecePosition
toLink :: (PiecePosition,PiecePosition)
          -> Set PiecePosition
toLink (x,y) = fromList [x,y]
{-# INLINE toLink #-}


connections :: Set Link
connections = fromList
              [ toLink (x,y)
              | x <- outer
              , y <- inner] S.\\ forbidden

forbidden :: Set Link
forbidden = fromList
            $ map toLink [(V,Four),(W,One),(X,Three),(Y,Two)]
{-# INLINE forbidden #-}

isConnection = (`member` connections)
{-# INLINE isConnection #-}

-- Yields True if the second position is obtainable by making a move
-- from the first.

isValidMove :: PlayerPosition -> PlayerPosition -> Bool
isValidMove x1 x2
  = isConnection $ (symmDiff `on` getPlayerPosition) x1 x2

-- Game record data. Three types are provided: a long and short
-- format, and an abbreviated format useful for determining whether a
-- position violates the "no-repeat" rule.

newtype GameRecord
  = GameRecord
    { getGameRecord :: [GlobalPosition]
    } deriving (Eq, Show)

newtype ShortGameRecord
  = ShortGameRecord
    { getShortGameRecord :: [PlayerPosition]
    } deriving Eq

-- Try to shorten the mess below:
class Validatable a where
  isValid :: a -> Bool
  embedMove :: GlobalPosition -> a -> a
  embedHalfMove :: PlayerPosition -> a -> a
  currentTurn :: a -> Player
  getCurrentPosition :: a -> GlobalPosition
  getFirstPlayerPosition :: a -> PlayerPosition
  getSecondPlayerPosition :: a -> PlayerPosition
  getPlayersPosition :: Player -> a -> PlayerPosition
  getCurrentPlayerPosition :: a -> PlayerPosition
  getWaitingPlayerPosition :: a -> PlayerPosition
  possibleMoves :: a -> [GlobalPosition]
  hasVictory :: a -> Maybe Player
  embedHalfMove p g = embedMove (GlobalPosition x) g
    where
      x = (if currentTurn g == Player1
           then id else swap) (p',p)
      p' = getWaitingPlayerPosition g
  getFirstPlayerPosition = fst . getGlobalPosition . getCurrentPosition
  getSecondPlayerPosition = snd . getGlobalPosition . getCurrentPosition
  getPlayersPosition Player1 = getFirstPlayerPosition
  getPlayersPosition Player2 = getSecondPlayerPosition
  getCurrentPlayerPosition g = getPlayersPosition (currentTurn g) g
  getWaitingPlayerPosition g = getPlayersPosition (switch (currentTurn g)) g
  hasVictory g
    | x1 == player2_start = Just Player1
    | x2 == player1_start = Just Player2
    | null $ possibleMoves g = Just (switch (currentTurn g))
    | otherwise = Nothing
    where GlobalPosition (x1,x2) = getCurrentPosition g

  possibleMoves g =
    let otherPosition = getWaitingPlayerPosition g
        (piece1,piece2) = fromPlayerPosition $ getCurrentPlayerPosition g
        imbed x = GlobalPosition
                  $(if currentTurn g == Player1 then id else swap)
                  $(x,otherPosition)
        goingFrom p = toList
                      $ unions
                      $ toList
                      $ S.map (S.\\ (S.singleton p))
                      $ S.filter (p `member`) connections
        rawMoves = map imbed $
                   [toPlayerPosition x1 piece2
                   | x1 <- goingFrom piece1]
                   ++ [toPlayerPosition piece1 x2
                      | x2 <- goingFrom piece2]
    in  filter (isValid . (flip embedMove g)) rawMoves

instance Validatable GameRecord where
  isValid (GameRecord x) = condition_1 x
                           && condition_2 x
                           && condition_3 x
                           && condition_4 x
  embedMove x (GameRecord m) = GameRecord (m++[x])
  currentTurn (GameRecord x) = toEnum $ ((length x)+1) `mod` 2
  getCurrentPosition (GameRecord x) = last x

instance Validatable ShortGameRecord where
  isValid x = let re = getShortGameRecord x
              in  condition_3 re
                  && condition_4 (map GlobalPosition (zip re (tail re)))
  embedMove x (ShortGameRecord m) = ShortGameRecord (m++[px])
    where px = getPairMod2 (length m) $ (getGlobalPosition x)
            where getPairMod2 n = if even n then snd else fst
  currentTurn (ShortGameRecord x) = toEnum $ ((length x)+1) `mod` 2
  getCurrentPosition g@(ShortGameRecord x)
    = GlobalPosition
      $ (if currentTurn g == Player1
         then id else swap)
      $ (last (init x),last x)
-- Conditions for testing validity.

-- Condition 1: The head of a game record must be the start position.

condition_1 = (== start) . head
{-# INLINE condition_1 #-}

-- Condition 2: In a game record, any two adjacent positions must
-- differ by exactly one player position, which itself must differ by
-- exactly one piece position. Furthermore, the first player's
-- position must differ between the first and second moves, the second
-- player's must differ between the second and third, etc.

condition_2 = and
              . (mapTwist test) -- see helper function below.
              . (ap zip tail)   -- this is equivalent to @\l -> zip l
                                -- (tail l)@
              . map getGlobalPosition
  where test ((x1,y1),(x2,y2)) = y1 == y2 && isValidMove x1 x2

-- Old version of function:
--
-- condition_2 [_] = True
-- condition_2 ((GlobalPosition (x1,y1)):ms@((GlobalPosition (x2,y2)):_))
--   = (y1 == y2 && isValidMove x1 x2) && condition_2 (map swap ms)
--   where swap (GlobalPosition (x,y)) = GlobalPosition (y,x)
--

condition_3 :: (Eq a) => [a] -> Bool
condition_3 = isUnique

{-# INLINE condition_3 #-}

condition_4 :: [GlobalPosition] -> Bool
condition_4 = getAll . foldMap (All . disjoint)
  where disjoint (GlobalPosition x)
          = S.null $ uncurry (S.intersection `on` getPlayerPosition) x

-- Helper functions for this section.

isUnique :: (Eq a) => [a] -> Bool
isUnique l = all doesNotContain (zip (inits l) l)
  where doesNotContain = uncurry (flip notElem)

symmDiff :: (Eq a, Ord a) => Set a -> Set a -> Set a
symmDiff set1 set2 =
  (S.union set1 set2) S.\\ (S.intersection set1 set2)

mapTwist :: (((a,a),(a,a)) -> b) -> [((a,a),(a,a))] -> [b]
mapTwist _ [] = []
mapTwist f (l0:ls) = f l0 : mapTwist f (map (\(x,y) -> (swap x,swap y)) ls)
