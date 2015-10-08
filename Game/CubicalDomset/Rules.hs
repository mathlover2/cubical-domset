module Game.CubicalDomset.Rules
       where

import Game.CubicalDomset.Notation

import Data.Foldable (foldMap)
import Data.Function (on)
import Data.List ( (\\), nub, inits )
import Control.Monad
import Data.Monoid (All(..))
import qualified Data.Set as Set
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

type Link = Set.Set PiecePosition
toLink :: [PiecePosition] -> Set.Set PiecePosition
toLink [x,y] = Set.fromList [x,y]
{-# INLINE toLink #-}


connections :: Set.Set Link
connections = Set.fromList
              [ toLink [x,y]
              | x <- outer
              , y <- inner] Set.\\ forbidden

forbidden :: Set.Set Link
forbidden = Set.fromList
            $ map toLink [[V,Four],[W,One],[X,Three],[Y,Two]]
{-# INLINE forbidden #-}

isConnection = (`Set.member` connections)
{-# INLINE isConnection #-}

-- Yields True if the second position is obtainable by making a move
-- from the first.

isValidMove :: PlayerPosition -> PlayerPosition -> Bool
isValidMove x1 x2
  = let s1 = getPlayerPosition x1
        s2 = getPlayerPosition x2
    in  isConnection (symmDiff s1 s2)

-- Game record data. Three types are provided: a long and short
-- format, and an abbreviated format useful for determining whether a
-- position violates the "no-repeat" rule.

newtype GameRecord
  = GameRecord
    {getGameRecord :: [GlobalPosition]
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
  getCurrentPlayerPosition :: a -> PlayerPosition
  getWaitingPlayerPosition :: a -> PlayerPosition
  possibleMoves :: a -> [GlobalPosition]
  hasVictory :: a -> Maybe Player
  embedHalfMove p g = embedMove (GlobalPosition x) g
    where
      x = case (currentTurn g)
          of Player1 -> (p,p')
             Player2 -> (p',p)
      p' = getWaitingPlayerPosition g
  getCurrentPlayerPosition g
    = (if currentTurn g == Player1 then fst else snd)
      $ getGlobalPosition
      $ getCurrentPosition g
  getWaitingPlayerPosition g
    = (if currentTurn g == Player1 then snd else fst)
      $ getGlobalPosition
      $ getCurrentPosition g
  hasVictory g
    = let GlobalPosition x = getCurrentPosition g
      in  case x of (x1,x2) | x1 == player2_start -> Just Player1
                            | x2 == player1_start -> Just Player2
                            | otherwise -> if null $ possibleMoves g
                                           then Just (switch (currentTurn g))
                                           else Nothing

  possibleMoves g = let otherPosition = getWaitingPlayerPosition g
                        (piece1,piece2) = fromPlayerPosition
                                          $ getCurrentPlayerPosition g
                        imbed x = GlobalPosition
                                  $(if currentTurn g == Player1 then id else swap)
                                  $(x,otherPosition)
                        goingFrom p = Set.toList
                                      $ Set.unions
                                      $ Set.toList
                                      $ Set.map (Set.\\ (Set.singleton p))
                                      $ Set.filter (p `Set.member`) connections
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
          = Set.null $ uncurry (Set.intersection `on` getPlayerPosition) x

-- Helper functions for this section.

isUnique :: (Eq a) => [a] -> Bool
isUnique l = all doesNotContain (zip (inits l) l)
  where doesNotContain = uncurry (flip notElem)

symmDiff :: (Eq a, Ord a) => Set.Set a -> Set.Set a -> Set.Set a
symmDiff set1 set2 =
  (Set.union set1 set2) Set.\\ (Set.intersection set1 set2)

mapTwist :: (((a,a),(a,a)) -> b) -> [((a,a),(a,a))] -> [b]
mapTwist _ [] = []
mapTwist f (l0:ls) = f l0 : mapTwist f (map (\(x,y) -> (swap x,swap y)) ls)
