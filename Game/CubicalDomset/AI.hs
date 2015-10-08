module Game.CubicalDomset.AI
       where

import Game.CubicalDomset.Notation
import Game.CubicalDomset.Rules


type CubicalDomsetAI = GameRecord -> GameRecord

-- The AI firstMove: a really bad AI.

firstMove :: CubicalDomsetAI
firstMove g = let x = head $ possibleMoves g in embedMove x g


