import Game.CubicalDomset.Rules
import Game.CubicalDomset.Notation
import Game.CubicalDomset.AI
import Data.Set (fromList)

main = playGameWithTwo (GameRecord [start])


playGameWithTwo g
  = do x <- makeMove g
       canContinue <- analyzeMove x
       if canContinue
         then playGameWithTwo x
         else return ()

makeMove :: GameRecord -> IO GameRecord
makeMove l
  = do putStrLn $ "Enter a move, " ++ (show (currentTurn l)) ++ ":"
       putStrLn $ "Move must be of the form [piece1,piece2],\n"
                   ++ "e.g, \"[V,W]\", not \"VW\""
       x <- fmap read getLine
       let lnew = (embedHalfMove
                   (PlayerPosition (fromList x))
                   l)
       if isValid lnew
         then return lnew
         else do putStrLn "Invalid move given"
                 makeMove l
analyzeMove l
  = case hasVictory l
    of Just player
         -> (putStrLn $ (show player) ++ "has won!")
            >> return False
       Nothing
         -> do (putStrLn
                $ "The current board position is "
                ++ show (getCurrentPosition l) ++ ".")
               (putStrLn
                $ "The moves available are as follows: "
                ++ show (possibleMoves l) ++ ".")
               return True


-- Playing with an AI.

-- playGameWithAI :: CubicalDomsetAI -> GameRecord -> IO
-- playGameWithAI ai = makeMoveWithAI ai (GameRecord [start])
