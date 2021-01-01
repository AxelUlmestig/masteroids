module Assets (
  Assets(..),
  loadAssets
) where

import           Graphics.Gloss (Picture (..), loadBMP, rotate)

data Assets = Assets {
  playerSprite   :: Picture,
  fireSprite     :: Picture,
  asteroidSprite :: Picture,
  laserSprite    :: Picture
}

loadAssets :: IO Assets
loadAssets = do
  playerS   <- rotate 90 <$> loadBMP "assets/sprites/playerShip2-blue.bmp"
  fireS     <- rotate 90 <$> loadBMP "assets/sprites/fire01.bmp"
  asteroidS <- loadBMP "assets/sprites/meteorBrown-big4.bmp"
  laserS    <- rotate 90 <$> loadBMP "assets/sprites/laserGreen13.bmp"
  return $ Assets playerS fireS asteroidS laserS
