module GameState (
  GameState(..),
  initGameState,
  gameStatePlayerPositionL,
  gameStatePlayerVelocityL,
  gameStatePlayerAngleL,
  gameStateMousePositionL,
  gameStateAcceleratingL
) where

import           Control.Lens (Lens', lens)

import           Vector       (Vector (..))

data GameState = GameState {
  playerPosition :: Vector Float,
  playerAngle    :: Float,
  playerVelocity :: Vector Float,
  mousePosition  :: Vector Float,
  accelerating   :: Bool
} deriving (Eq, Show)

initGameState :: GameState
initGameState = GameState (Vector 0 0) 0 (Vector 0 0) (Vector 0 0) False

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
