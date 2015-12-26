module Game.CubicalDomset.Gamespace
       where

import Data.Maybe (isNothing)
import Game.CubicalDomset.Rules
import Game.CubicalDomset.Notation
import System.Random

initialGame = GameRecord [start]

successors :: GameRecord -> [GameRecord]
successors g = map (flip embedMove g) (possibleMoves g)

predecessor (GameRecord x) = GameRecord (init x)

listOfAllGamesOf :: GameRecord -> [GameRecord]
listOfAllGamesOf g = analyzeGames g
  where analyzeGames g
          = do x <- successors g
               if isFinished x
                 then return x
                 else analyzeGames x

progressionsFrom g = iterate (concat . map successors) [g]

listOfAllGames = listOfAllGamesOf initialGame

localImage fwd bwd
  = concat
    . map (\x -> progressionsFrom x !! fwd)
    . return
    . (\x -> iterate predecessor x !! bwd)

gameLength (GameRecord x) = length x

isFinished :: GameRecord -> Bool
isFinished = not . isNothing . hasVictory

randomGame = build initialGame
  where build g = if isFinished g
                  then return g
                  else do x <- randomElement (successors g)
                          build x

-- The helper function randomElement.

randomElement l = let lnn = length l
                  in  do r <- randomRIO (0,lnn-1)
                         return (l !! r)
