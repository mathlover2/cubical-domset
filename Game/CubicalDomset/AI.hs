module Game.CubicalDomset.AI
       where

import Game.CubicalDomset.Gamespace
import Game.CubicalDomset.Notation
import Game.CubicalDomset.Rules
import System.Random

type CubicalDomsetAI = GameRecord -> IO GameRecord

-- The AI firstMove: a really bad AI -- as in utterly terrible.
-- As the name suggests, it picks the first move (the head) of
-- the list @possibleMoves g@. It is trivial to beat -- it does
-- not even bother to defend itself from losing -- and was only
-- written to test the AI interface, and potentially as a combinator
-- for constructing other AI, per a system I may ultimately design.

firstMove :: CubicalDomsetAI
firstMove g = let x = head $ possibleMoves g in return (embedMove x g)

-- The AI randomMove: an AI performing exactly as chance would expect.

randomMove :: CubicalDomsetAI
randomMove g = do x <- randomElement $ possibleMoves g
                  return (embedMove x g)

-- The helper function randomElement.

randomElement l = let lnn = length l
                  in  do r <- randomRIO (0,lnn-1)
                         return (l !! r)


