module Render (render) where

import           Control.Lens    (at, imap, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Graphics.Gloss  (Picture (Blank, Pictures), rotate, scale,
                                  translate)

import           Assets          (Assets, asteroidSprite, fireSprite,
                                  laserSprite, playerSprite)
import qualified Constants
import           GameState       (EntityType (Asteroid, Laser, Player),
                                  GameState, accelerating, gameHeight,
                                  gameStateAnglesL, gameStateEntityTypesL,
                                  gameStatePositionsL, gameStateRadiiL,
                                  gameWidth)
import           Physics         (Angle (Angle), toPair)

render :: Assets -> GameState -> Picture
render assets gs = renderLoopedSpace gs
                  $ offsetGameWindow gs
                  $ Pictures . fmap snd . M.toList
                  $ setSpritePositions gs
                  $ rotateSprites gs
                  $ getSprites assets gs

-- Gloss puts the origin in the middle of the screen by default. This puts it in the lower left corner
offsetGameWindow :: GameState -> Picture -> Picture
offsetGameWindow gs = translate (fromIntegral (gameWidth gs) * (-0.5)) (fromIntegral (gameHeight gs) * (-0.5))

-- Render the edges of entities as they "wrap around" the edges of the map
renderLoopedSpace :: GameState -> Picture -> Picture
renderLoopedSpace gs pic = let
                             xMax = fromIntegral $ gameWidth gs
                             yMax = fromIntegral $ gameHeight gs
                           in Pictures $ translate <$> [-xMax, 0, xMax] <*> [-yMax, 0, yMax] <*> return pic

setSpritePositions :: GameState -> M.Map Int Picture -> M.Map Int Picture
setSpritePositions gs = M.intersectionWith (uncurry translate . toPair) (view gameStatePositionsL gs)

rotateSprites :: GameState -> M.Map Int Picture -> M.Map Int Picture
rotateSprites gs = M.intersectionWith (rotate . negate . unwrap) (view gameStateAnglesL gs)
  where
    unwrap (Angle ang) = ang

getSprites :: Assets -> GameState -> M.Map Int Picture
getSprites assets gs = imap f $ view gameStateEntityTypesL gs
  where
    f _ Laser       = laserSprite assets
    f eid Asteroid  = fromMaybe Blank $ do
                        rad <- view (gameStateRadiiL . at eid) gs
                        let s = rad / Constants.asteroidSpriteDefaultRadius
                        return $ scale s s $ asteroidSprite assets
    f _ Player      = if not (accelerating gs)
                      then playerSprite assets
                      else Pictures [playerSprite assets, translate (-50) 0 (fireSprite assets)]
