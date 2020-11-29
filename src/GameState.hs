module GameState (
  GameState(..),
  playerId,
  initGameState,
  gameStateGameWidthL,
  gameStateGameHeightL,
  gameStatePositionsL,
  gameStateVelocitiesL,
  gameStateAnglesL,
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
  angles        :: M.Map Int Float,
  mousePosition :: Vector Float,
  accelerating  :: Bool
} deriving (Eq, Show)

initGameState :: GameState
initGameState = GameState {
    gameWidth       = defaultWidth,
    gameHeight      = defaultHeight,
    positions       = M.fromList [(playerId, Vector 0 0)],
    velocities      = M.fromList [(playerId, Vector 0 0)],
    angles          = M.fromList [(playerId, 0)],
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

gameStateAnglesL :: Lens' GameState (M.Map Int Float)
gameStateAnglesL = lens angles (\gs as -> gs { angles = as })

gameStateMousePositionL :: Lens' GameState (Vector Float)
gameStateMousePositionL = lens mousePosition (\gs mp -> gs { mousePosition = mp })

gameStateAcceleratingL :: Lens' GameState Bool
gameStateAcceleratingL = lens accelerating (\gs a -> gs { accelerating = a })
