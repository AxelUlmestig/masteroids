module GameState (
  GameState(..),
  initGameState,
  gameStateGameWidthL,
  gameStateGameHeightL,
  gameStatePlayerPositionL,
  gameStatePlayerVelocityL,
  gameStatePlayerAngleL,
  gameStateMousePositionL,
  gameStateAcceleratingL,
  defaultWidth,
  defaultHeight
) where

import           Control.Lens (Lens', lens)

import           Vector       (Vector (..))

defaultWidth :: Int
defaultWidth = 800

defaultHeight :: Int
defaultHeight = 800

data GameState = GameState {
  gameWidth      :: Int,
  gameHeight     :: Int,
  playerPosition :: Vector Float,
  playerAngle    :: Float,
  playerVelocity :: Vector Float,
  mousePosition  :: Vector Float,
  accelerating   :: Bool
} deriving (Eq, Show)

initGameState :: GameState
initGameState = GameState {
    gameWidth       = defaultWidth,
    gameHeight      = defaultHeight,
    playerPosition  = Vector 0 0,
    playerAngle     = 0,
    playerVelocity  = Vector 0 0,
    mousePosition   = Vector 0 0,
    accelerating    = False
  }

gameStateGameWidthL :: Lens' GameState Int
gameStateGameWidthL = lens gameWidth (\gs gw -> gs { gameWidth = gw })

gameStateGameHeightL :: Lens' GameState Int
gameStateGameHeightL = lens gameWidth (\gs gw -> gs { gameWidth = gw })

gameStatePlayerPositionL :: Lens' GameState (Vector Float)
gameStatePlayerPositionL = lens playerPosition (\gs pp -> gs { playerPosition = pp })

gameStatePlayerAngleL :: Lens' GameState Float
gameStatePlayerAngleL = lens playerAngle (\gs pa -> gs { playerAngle = pa })

gameStatePlayerVelocityL :: Lens' GameState (Vector Float)
gameStatePlayerVelocityL = lens playerVelocity (\gs pv -> gs { playerVelocity = pv })

gameStateMousePositionL :: Lens' GameState (Vector Float)
gameStateMousePositionL = lens mousePosition (\gs mp -> gs { mousePosition = mp })

gameStateAcceleratingL :: Lens' GameState Bool
gameStateAcceleratingL = lens accelerating (\gs a -> gs { accelerating = a })
