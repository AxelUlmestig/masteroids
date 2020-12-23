module Render (render) where

import           Control.Lens    (at, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (mapMaybe)
import           Graphics.Gloss  (Picture (Blank, Pictures), rotate, translate)

import           Assets          (Assets, asteroidSprite, fireSprite,
                                  laserSprite, playerSprite)
import           GameState       (EntityType (Asteroid, Laser, Player),
                                  GameState, accelerating, gameHeight,
                                  gameStateAnglesL, gameStateEntityTypesL,
                                  gameStatePositionsL, gameWidth)
import           Physics         (Angle (Angle), Position, createV,
                                  movePosition, rotateV, toPair)

data RenderData = PlayerRender Position Angle Bool
                | AsteroidRender Position Angle
                | LaserRender Position Angle

render :: Assets -> GameState -> Picture
render assets gs = renderLoopedSpace gs $ offsetGameWindow gs $ Pictures $ renderEntity assets <$> allRenderData gs

allRenderData :: GameState -> [RenderData]
allRenderData gs = let
                     mPos eid           = view (gameStatePositionsL . at eid) gs
                     mAng eid           = view (gameStateAnglesL . at eid) gs
                     f (eid, Player)    = PlayerRender <$> mPos eid <*> mAng eid <*> return (accelerating gs)
                     f (eid, Asteroid)  = AsteroidRender <$> mPos eid <*> mAng eid
                     f (eid, Laser)     = LaserRender <$> mPos eid <*> mAng eid
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
renderEntity assets (LaserRender pos (Angle ang)) = translate x y (rotate (-ang) (laserSprite assets))
  where
    (x, y) = toPair pos

-- Gloss puts the origin in the middle of the screen by default. This puts it in the lower left corner
offsetGameWindow :: GameState -> Picture -> Picture
offsetGameWindow gs = translate (fromIntegral (gameWidth gs) * (-0.5)) (fromIntegral (gameHeight gs) * (-0.5))

-- Render the edges of entities as they "wrap around" the edges of the map
renderLoopedSpace :: GameState -> Picture -> Picture
renderLoopedSpace gs pic = let
                             xMax = fromIntegral $ gameWidth gs
                             yMax = fromIntegral $ gameHeight gs
                           in Pictures $ translate <$> [-xMax, 0, xMax] <*> [-yMax, 0, yMax] <*> return pic
