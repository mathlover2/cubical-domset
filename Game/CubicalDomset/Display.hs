{-# LANGUAGE NoMonomorphismRestriction #-}

module Game.CubicalDomset.Display
       where

import Game.CubicalDomset.Notation
import Game.CubicalDomset.Rules hiding (inner, outer)
import qualified Game.CubicalDomset.Rules as Gameplay (inner,outer)

import Diagrams.Prelude
import Data.List ((\\))
import qualified Data.Set as Set


positionList
  = map p2 [(-0.5,1),(0.5,1),(0.33,0),(-0.33,0),(-0.5,-1),(0.5,-1),(1,0),(-1,0)]

[positV,positW,posit3,posit2,positX,positY,posit4,posit1] = positionList

toPosition x
  = case x
    of V -> positV
       W -> positW
       X -> positX
       Y -> positY
       One -> posit1
       Two -> posit2
       Three -> posit3
       Four -> posit4

posit = circle 0.2

places = atPoints positionList (repeat posit) # fc white

connectingLines
  = map (uncurry (~~)) 
    ([(toPosition x, toPosition y)
     | x <- Gameplay.inner
     , y <- Gameplay.outer] 
     \\ [(positV,posit4),(positW,posit1),(positX,posit3),(positY,posit2)])

board = places `atop` (mconcat connectingLines)

depictGamePosition (x,y) = (player1 # fc red)
                           `atop` (player2 # fc blue)
                           `atop` board
  where [player1,player2] = map depict [x,y]
        depict s = atPoints (map toPosition (Set.elems s)) (repeat posit)
                                  
