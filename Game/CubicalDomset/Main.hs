import Game.CubicalDomset.Rules
import Game.CubicalDomset.Notation
import Game.CubicalDomset.AI
import System.Environment (getArgs)
import Data.Set (fromList)

main = do x <- getArgs
          if x == ["-a"]
            then playGameWithAI randomMove (GameRecord [start]) True
            else playGameWithTwo (GameRecord [start])


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



playGameWithAI :: CubicalDomsetAI
               -> GameRecord
               -> Bool -- is AI the current player?
               -> IO ()
playGameWithAI ai g b
  = do x <- (if b then ai else makeMove) g
       canContinue <- analyzeMove x
       if canContinue
         then playGameWithAI ai x (not b)
         else return () 
