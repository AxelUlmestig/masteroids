module GameState (
  GameState(..),
  initGameState,
  gameStatePlayerPositionL,
  gameStatePlayerAngleL,
  gameStateMousePositionL
) where

import           Control.Lens (Lens', lens)

import           Vector       (Vector (..))

data GameState = GameState {
  playerPosition :: Vector Float,
  playerAngle    :: Float,
  playerVelocity :: Vector Float,
  mousePosition  :: Vector Float
} deriving (Eq, Show)

initGameState :: GameState
initGameState = GameState (Vector 0 0) 0 (Vector 0 0) (Vector 0 0)

gameStatePlayerPositionL :: Lens' GameState (Vector Float)
gameStatePlayerPositionL = lens playerPosition (\gs pp -> gs { playerPosition = pp })

gameStatePlayerAngleL :: Lens' GameState Float
gameStatePlayerAngleL = lens playerAngle (\gs pa -> gs { playerAngle = pa })

gameStateMousePositionL :: Lens' GameState (Vector Float)
gameStateMousePositionL = lens mousePosition (\gs mp -> gs { mousePosition = mp })
