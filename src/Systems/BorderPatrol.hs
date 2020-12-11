module Systems.BorderPatrol (borderPatrol) where

import           Control.Lens (over)
import           Data.Fixed   (mod')

import           GameState    (GameState, gameHeight, gameStatePositionsL,
                               gameWidth)
import           Physics      (createV, toPair)

borderPatrol :: GameState -> GameState
borderPatrol gs = let
                     constrainPosition pos = createV (x `mod'` width) (y `mod'` height)
                      where
                        (x, y)  = toPair pos :: (Float, Float)
                        width   = fromIntegral (gameWidth gs) :: Float
                        height  = fromIntegral (gameHeight gs) :: Float
                   in over gameStatePositionsL (fmap constrainPosition) gs
