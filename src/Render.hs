module Render (render) where

import           Control.Lens    (at, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (mapMaybe)
import           Graphics.Gloss  (Picture (Blank, Pictures), rotate, translate)

import           Assets          (Assets, asteroidSprite, fireSprite,
                                  playerSprite)
import           GameState       (EntityType (Asteroid, Player), GameState,
                                  accelerating, gameStateAnglesL,
                                  gameStateEntityTypesL, gameStatePositionsL)
import           Physics         (Angle (Angle), Position, createV,
                                  movePosition, rotateV, toPair)

data RenderData = PlayerRender Position Angle Bool
                | AsteroidRender Position Angle

render :: Assets -> GameState -> Picture
render assets = Pictures . fmap (renderEntity assets) . allRenderData

allRenderData :: GameState -> [RenderData]
allRenderData gs = let
                     mPos eid           = view (gameStatePositionsL . at eid) gs
                     mAng eid           = view (gameStateAnglesL . at eid) gs
                     f (eid, Player)    = PlayerRender <$> mPos eid <*> mAng eid <*> return (accelerating gs)
                     f (eid, Asteroid)  = AsteroidRender <$> mPos eid <*> mAng eid
                   in mapMaybe f . M.toList . view gameStateEntityTypesL $ gs

renderEntity :: Assets -> RenderData -> Picture
renderEntity assets (PlayerRender pos (Angle ang) acc) = Pictures [player, fire]
  where
    (x, y) = toPair pos
    player = translate x y (rotate (-ang) (playerSprite assets))
    fire = if acc then translate x' y' (rotate (-ang) (fireSprite assets)) else Blank
      where
        (x', y') = toPair $ movePosition (rotateV (Angle (180 + ang)) (createV 50 0)) pos
renderEntity assets (AsteroidRender pos (Angle ang)) = translate x y (rotate (-ang) (asteroidSprite assets))
  where
    (x, y) = toPair pos
