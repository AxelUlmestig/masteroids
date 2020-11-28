module Render (render) where

import           Control.Lens   (at, view)
import           Data.Maybe     (fromJust)
import           Graphics.Gloss (Picture (Blank, Pictures), rotate, translate)

import           Assets         (Assets, fireSprite, playerSprite)
import           GameState      (GameState, accelerating, gameStatePositionsL,
                                 playerAngle, playerId)
import           Vector         (Vector (Vector), addV, rotateV)

render :: Assets -> GameState -> Picture
-- render assets GameState{ playerPosition = (Vector x y), playerAngle = a, accelerating = acc } = Pictures [player, fire]
render assets gs = Pictures [player, fire]
  where
    (Vector x y) = fromJust $ view (gameStatePositionsL . at playerId) gs -- will crash if player doesn't have a position
    ang = playerAngle gs
    player = translate x y (rotate (-ang) (playerSprite assets))
    fire = if accelerating gs then translate x' y' (rotate (-ang) (fireSprite assets)) else Blank
      where
        (Vector x' y') = addV (Vector x y) (rotateV (180 + ang) (Vector 50 0))
