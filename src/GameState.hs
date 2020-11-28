module GameState (
  GameState(..),
  playerId,
  initGameState,
  gameStateGameWidthL,
  gameStateGameHeightL,
  gameStatePositionsL,
  gameStateVelocitiesL,
  gameStatePlayerAngleL,
  gameStateMousePositionL,
  gameStateAcceleratingL,
  defaultWidth,
  defaultHeight
) where

import           Control.Lens    (Lens', lens)
import qualified Data.Map.Strict as M

import           Vector          (Vector (..))

defaultWidth :: Int
defaultWidth = 800

defaultHeight :: Int
defaultHeight = 800

playerId :: Int
playerId = 1

data GameState = GameState {
  gameWidth     :: Int,
  gameHeight    :: Int,
  positions     :: M.Map Int (Vector Float),
  velocities    :: M.Map Int (Vector Float),
  playerAngle   :: Float,
  mousePosition :: Vector Float,
  accelerating  :: Bool
} deriving (Eq, Show)

initGameState :: GameState
initGameState = GameState {
    gameWidth       = defaultWidth,
    gameHeight      = defaultHeight,
    positions       = M.fromList [(playerId, Vector 0 0)],
    velocities      = M.fromList [(playerId, Vector 0 0)],
    playerAngle     = 0,
    mousePosition   = Vector 0 0,
    accelerating    = False
  }

gameStateGameWidthL :: Lens' GameState Int
gameStateGameWidthL = lens gameWidth (\gs gw -> gs { gameWidth = gw })

gameStateGameHeightL :: Lens' GameState Int
gameStateGameHeightL = lens gameWidth (\gs gw -> gs { gameWidth = gw })

gameStatePositionsL :: Lens' GameState (M.Map Int (Vector Float))
gameStatePositionsL = lens positions (\gs ps -> gs { positions = ps })

gameStateVelocitiesL :: Lens' GameState (M.Map Int (Vector Float))
gameStateVelocitiesL = lens velocities (\gs vs -> gs { velocities = vs })

gameStatePlayerAngleL :: Lens' GameState Float
gameStatePlayerAngleL = lens playerAngle (\gs pa -> gs { playerAngle = pa })

gameStateMousePositionL :: Lens' GameState (Vector Float)
gameStateMousePositionL = lens mousePosition (\gs mp -> gs { mousePosition = mp })

gameStateAcceleratingL :: Lens' GameState Bool
gameStateAcceleratingL = lens accelerating (\gs a -> gs { accelerating = a })
