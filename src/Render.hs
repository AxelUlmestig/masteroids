module Render (render) where

import           Graphics.Gloss (Picture (Blank, Pictures), rotate, translate)

import           Assets         (Assets, fireSprite, playerSprite)
import           GameState      (GameState (GameState), accelerating,
                                 playerAngle, playerPosition)
import           Vector         (Vector (Vector), addV, rotateV)

render :: Assets -> GameState -> Picture
render assets GameState{ playerPosition = (Vector x y), playerAngle = a, accelerating = acc } = Pictures [player, fire]
  where
    player = translate x y (rotate (-a) (playerSprite assets))
    fire = if acc then translate x' y' (rotate (-a) (fireSprite assets)) else Blank
      where
        (Vector x' y') = addV (Vector x y) (rotateV (180 + a) (Vector 50 0))
