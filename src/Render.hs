module Render (render) where

import           Control.Lens    (at, imap, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust)
import           Graphics.Gloss  (Picture (Blank, Pictures), rotate, translate)

import           Assets          (Assets, fireSprite, playerSprite)
import           GameState       (EntityType (Asteroid, Player), GameState,
                                  accelerating, gameStateAnglesL,
                                  gameStateEntityTypesL, gameStatePositionsL)
import           Vector          (Vector (Vector), addV, rotateV)

render :: Assets -> GameState -> Picture
render assets gs = Pictures . fmap snd . M.toList . imap f . view gameStateEntityTypesL $ gs
  where
    f eid Player = renderPlayer assets eid gs
    f _ Asteroid = Blank

renderPlayer :: Assets -> Int -> GameState -> Picture
renderPlayer assets eid gs = Pictures [player, fire]
  where
    (Vector x y) = fromJust $ view (gameStatePositionsL . at eid) gs -- will crash if player doesn't have a position
    ang = fromJust $ view (gameStateAnglesL . at eid) gs -- will crash if player doesn't have an angle
    player = translate x y (rotate (-ang) (playerSprite assets))
    fire = if accelerating gs then translate x' y' (rotate (-ang) (fireSprite assets)) else Blank
      where
        (Vector x' y') = addV (Vector x y) (rotateV (180 + ang) (Vector 50 0))
