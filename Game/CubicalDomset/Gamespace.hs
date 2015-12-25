module Game.CubicalDomset.Gamespace
       where

import Game.CubicalDomset.Rules
import Game.CubicalDomset.Notation

successors :: GameRecord -> [GameRecord]
successors g = map (flip embedMove g) (possibleMoves g)

predecessor = GameRecord . init . getGameRecord 

listOfAllGamesOf :: GameRecord -> [GameRecord]
listOfAllGamesOf g = analyzeGames g
  where analyzeGames g
          = do x <- successors g
               if null $ possibleMoves x
                 then return x
                 else analyzeGames x

progressionsFrom g = iterate (concat . map successors) [g]

listOfAllGames = listOfAllGamesOf (GameRecord [start])

localImage fwd bwd
  = concat
    . map (\x -> progressionsFrom x !! fwd)
    . return
    . (\x -> iterate predecessor x !! bwd)
