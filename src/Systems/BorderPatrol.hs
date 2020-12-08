module Systems.BorderPatrol (borderPatrol) where

import           Control.Lens (over)
import           Data.Fixed   (mod')

import           GameState    (GameState, gameHeight, gameStatePositionsL,
                               gameWidth)
import           Physics      (createV, toPair)

borderPatrol :: GameState -> GameState
borderPatrol gs = let
                     constrainPosition pos = createV (constrain (gameWidth gs) x) (constrain (gameHeight gs) y)
                       where
                         (x, y) = toPair pos

                     constrain width value = ((value + limit) `mod'` fromIntegral width) - limit
                       where
                         limit = fromIntegral width / 2
                   in over gameStatePositionsL (fmap constrainPosition) gs
