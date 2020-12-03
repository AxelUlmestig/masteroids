module Render (render) where

import           Control.Lens    (at, imap, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust)
import           Graphics.Gloss  (Picture (Blank, Pictures), rotate, translate)

import           Assets          (Assets, asteroidSprite, fireSprite,
                                  playerSprite)
import           GameState       (EntityType (Asteroid, Player), GameState,
                                  accelerating, gameStateAnglesL,
                                  gameStateEntityTypesL, gameStatePositionsL)
import           Physics         (Angle (Angle), createV, movePosition, rotateV,
                                  toPair)

render :: Assets -> GameState -> Picture
render assets gs = Pictures . fmap snd . M.toList . imap f . view gameStateEntityTypesL $ gs
  where
    f eid Player   = renderPlayer assets eid gs
    f eid Asteroid = renderAsteroid assets eid gs

renderPlayer :: Assets -> Int -> GameState -> Picture
renderPlayer assets eid gs = Pictures [player, fire]
  where
    pos = fromJust $ view (gameStatePositionsL . at eid) gs -- will crash if player doesn't have a position
    (x, y) = toPair pos
    (Angle ang) = fromJust $ view (gameStateAnglesL . at eid) gs -- will crash if player doesn't have an angle
    player = translate x y (rotate (-ang) (playerSprite assets))
    fire = if accelerating gs then translate x' y' (rotate (-ang) (fireSprite assets)) else Blank
      where
        (x', y') = toPair $ movePosition (rotateV (Angle (180 + ang)) (createV 50 0)) pos

renderAsteroid :: Assets -> Int -> GameState -> Picture
renderAsteroid assets eid gs = translate x y (rotate (-ang) (asteroidSprite assets))
  where
    (x, y) = toPair $ fromJust $ view (gameStatePositionsL . at eid) gs -- will crash if entity doesn't have a position
    (Angle ang) = fromJust $ view (gameStateAnglesL . at eid) gs -- will crash if entity doesn't have an angle
