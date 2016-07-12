{-# LANGUAGE ViewPatterns, PatternSynonyms, PatternGuards #-}

module Game.CubicalDomset.Rules
       where

import Game.CubicalDomset.Notation

import Data.Foldable (foldMap)
import Data.Function (on)
import Data.List (nub, inits, sort)
import Control.Monad
import Data.Monoid (All(..))
import Data.Set (empty, fromList, Set,
                 member, notMember, intersection, union,
                 toList, singleton, (\\))
import qualified Data.Set as S
import Data.Tuple (swap)

{-| Basic infrastructure -}

-- Player datatype.

data Player = Player1 | Player2 deriving (Eq, Enum, Ord, Read, Show)

switch :: Player -> Player
switch Player1 = Player2
switch Player2 = Player1
{-# INLINE switch #-}

-- Starting position and victory condition tests.

player1_start, player2_start :: PlayerPosition
player1_start = toPlayerPosition V W
player2_start = toPlayerPosition X Y
{-# INLINE player1_start #-}
{-# INLINE player2_start #-}

pattern Player1Start <- ((== player1_start) -> True)
pattern Player2Start <- ((== player2_start) -> True)
pattern GlobalPositionOf p1 p2 = GlobalPosition (p1,p2)
pattern PlayerPositionOf x y <- (fromPlayerPosition -> (x,y))

start :: GlobalPosition
start = toGlobalPosition player1_start player2_start
{-# INLINE start #-}

-- Connections between positions. A position is accessible to another
-- if it is connected by a line on the gameboard.

inner = [V,W,X,Y]
{-# INLINE inner #-}
outer = [One,Two,Three,Four]
{-# INLINE outer #-}

type Link = PlayerPosition

toLink :: (PiecePosition,PiecePosition)
          -> Link
toLink = uncurry toPlayerPosition
{-# INLINE toLink #-}


connections :: Set Link
connections = fromList [toLink (V,One),toLink (V,Two),
                        toLink (V,Three),toLink (W,Two),
                        toLink (W,Three),toLink (W,Four),
                        toLink (X,One),toLink (X,Two),
                        toLink (X,Four),toLink (Y,One),
                        toLink (Y,Three),toLink (Y,Four)]
{-# INLINE connections #-}

forbidden :: Set Link
forbidden = fromList
            $ map toLink [(V,Four),(W,One),(X,Three),(Y,Two)]
{-# INLINE forbidden #-}

goingFrom :: PiecePosition -> [PiecePosition]
goingFrom V = [One,Two,Three]
goingFrom W = [Two,Three,Four]
goingFrom X = [One,Two,Four]
goingFrom Y = [One,Three,Four]
goingFrom One = [V,X,Y]
goingFrom Two = [V,W,X]
goingFrom Three = [V,W,Y]
goingFrom Four = [W,X,Y]

listOfPositions piece1 piece2
  =  [toPlayerPosition x1 piece2 | x1 <- goingFrom piece1, x1 /= piece2]
     ++ [toPlayerPosition piece1 x2 | x2 <- goingFrom piece2, x2 /= piece1]

{-# INLINABLE listOfPositions #-}

{-# INLINE goingFrom #-}



isConnection p
  | (x,y) <- fromPlayerPosition p = x `elem` inner
                                    && y `elem` outer
                                    && p `notMember` forbidden
{-# INLINE isConnection #-}

-- Yields True if the second position is obtainable by making a move
-- from the first.

isValidMove :: PlayerPosition -> PlayerPosition -> Bool
isValidMove p1 p2
  = case (fromPlayerPosition p1,fromPlayerPosition p2)
    of   ((a,b),(c,d))
           -> case removeDuplicates [a,b,c,d]
              of [x,y] -> isConnection (toPlayerPosition x y)
                 _ -> False

-- Game record data. Three types are provided: a long and short
-- format, and an abbreviated format useful for determining whether a
-- position violates the "no-repeat" rule.

data GameRecord
  = GameRecord
    { getGameRecord :: [GlobalPosition]
    , isFirstPlayerTurn :: !Bool
    } deriving (Eq, Show)

-- Try to shorten the mess below:
class Validatable a where
  isValid :: a -> Bool
  isValid2 :: a -> Bool
  isValid3 :: a -> Bool
  embedMove :: GlobalPosition -> a -> a
  embedHalfMove :: PlayerPosition -> a -> a
  currentTurn :: a -> Player
  getCurrentPosition :: a -> GlobalPosition
  getFirstPlayerPosition :: a -> PlayerPosition
  getSecondPlayerPosition :: a -> PlayerPosition
  getPlayersPosition :: Player -> a -> PlayerPosition
  getCurrentPlayerPosition :: a -> PlayerPosition
  getWaitingPlayerPosition :: a -> PlayerPosition
  possibleMoves,possibleMoves' :: a -> [GlobalPosition]
  hasVictory :: a -> Maybe Player

  embedHalfMove p g = embedMove (GlobalPosition x) g
    where x = imbed swap (currentTurn g == Player1)
              (getWaitingPlayerPosition g) p

  getFirstPlayerPosition = fst . getGlobalPosition . getCurrentPosition
  getSecondPlayerPosition = snd . getGlobalPosition . getCurrentPosition

  getPlayersPosition Player1 = getFirstPlayerPosition
  getPlayersPosition Player2 = getSecondPlayerPosition

  getCurrentPlayerPosition g = getPlayersPosition (currentTurn g) g
  getWaitingPlayerPosition g = getPlayersPosition (switch (currentTurn g)) g

  hasVictory (getCurrentPosition -> GlobalPosition (Player2Start,_))
    = Just Player1
  hasVictory (getCurrentPosition -> GlobalPosition (_,Player1Start))
    = Just Player2
  hasVictory g = if null (possibleMoves g)
                 then Just $ switch $ currentTurn g
                 else Nothing

  possibleMoves g =
    let otherPosition = getWaitingPlayerPosition g
        PlayerPositionOf piece1 piece2 = getCurrentPlayerPosition g
        imbed' x = imbed GlobalPosition (currentTurn g == Player1)
                   x otherPosition
        rawMoves = map imbed' $ listOfPositions piece1 piece2
    in  filter (isValid2 . (flip embedMove g)) rawMoves
  possibleMoves' g =
    let otherPosition = getWaitingPlayerPosition g
        PlayerPositionOf piece1 piece2 = getCurrentPlayerPosition g
        imbed' x = imbed GlobalPosition (currentTurn g == Player1)
                   x otherPosition
        rawMoves = map imbed' $ listOfPositions piece1 piece2
    in  filter (isValid3 . (flip embedMove g)) rawMoves
  
instance Validatable GameRecord where
  isValid g@(GameRecord x b) = condition_1 x && isValid2 g
  isValid2 (GameRecord x b) = condition_3 x && condition_2 b x
  isValid3 (GameRecord x b) = condition_2a b x && condition_3 x
  embedMove x (GameRecord m b) = GameRecord (x:m) (not b)
  currentTurn (GameRecord _ b) = if b then Player1 else Player2
  getCurrentPosition (GameRecord x _) = head x

-- Condition 1: The first position in a game record must be the start position.

condition_1 = (== start) . last
{-# INLINE condition_1 #-}

-- Condition 2: In a game record, any two adjacent positions must
-- differ by exactly one player position, which itself must differ by
-- exactly one piece position. Furthermore, the first player's
-- position must differ between the first and second moves, the second
-- player's must differ between the second and third, etc. Finally, no
-- two pieces share the same position.

condition_2 b = and
                . (mapTwist test) -- see helper function below.
                . map (if b then (\(x,y) -> (swap x, swap y)) else id)
                . (\l -> zip l (tail l))
                . map getGlobalPosition

test ((x1,y1),(x2,y2)) = y1 == y2
                         && isDisjoint x1 y1
                         && isDisjoint x2 y1
                         && isValidMove x1 x2
isDisjoint a b =
  let (a1,a2) = fromPlayerPosition a
      (b1,b2) = fromPlayerPosition b
  in  isUnique [a1,a2,b1,b2]

{-# INLINABLE condition_2 #-}
{-# INLINE test #-}
{-# INLINE isDisjoint #-}

condition_2a b x
  = let z = (if b then (\(x,y) -> (swap x, swap y)) else id)
            $ head
            $ (\x -> zip x (tail x))
            $ map getGlobalPosition $ take 2 x
    in  test z

-- Old version of function:
--
-- condition_2 [_] = True
-- condition_2 ((GlobalPosition (x1,y1)):ms@((GlobalPosition (x2,y2)):_))
--   = (y1 == y2 && isValidMove x1 x2) && condition_2 (map swap ms)
--   where swap (GlobalPosition (x,y)) = GlobalPosition (y,x)
--

{- Condition 3 : All positions are unique -}

condition_3 :: [GlobalPosition] -> Bool
condition_3 = isUnique

{-# INLINE condition_3 #-}
             
-- Helper functions for this module.

isUnique :: (Eq a) => [a] -> Bool
isUnique l = nub l == l
{-# INLINABLE isUnique #-}

symmDiff :: (Eq a, Ord a) => Set a -> Set a -> Set a
symmDiff set1 set2 =
  (union set1 set2) \\ (intersection set1 set2)

{-# INLINABLE symmDiff #-}

mapTwist :: (((a,a),(a,a)) -> b) -> [((a,a),(a,a))] -> [b]
mapTwist _ [] = []
mapTwist f (l0:ls)
  = f l0 : mapTwist f (map (\(x,y) -> (swap x,swap y)) ls)

{-# INLINABLE mapTwist #-}

inConnection piece conn
  = let (x,y) = fromPlayerPosition conn
    in  piece == x || piece == y
        
{-# INLINE inConnection #-}

remove conn piece
  = case fromPlayerPosition conn
    of (x,y) | x == piece -> singleton y
             | y == piece -> singleton x
             | otherwise -> fromList [x,y]
{-# INLINE remove #-}

imbed f b a1 a2
  = f $ (if b then id else swap) (a1,a2)
{-# INLINE imbed #-}

removeDuplicates = f . sort
  where f [] = []
        f (a:b:xs) = if a == b then xs else a : f (b : xs)
        f x = x 

{-# INLINE removeDuplicates #-}
