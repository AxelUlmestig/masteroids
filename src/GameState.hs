module GameState (
  GameState(..),
  EntityType(..),
  initGameState,
  gameStateGameWidthL,
  gameStateGameHeightL,
  gameStateEntityTypesL,
  gameStatePositionsL,
  gameStateVelocitiesL,
  gameStateAnglesL,
  gameStateSpinL,
  gameStateMousePositionL,
  gameStateAcceleratingL,
  defaultWidth,
  defaultHeight,
  addEntity,
  destroyEntity
) where

import           Control.Lens    (Lens', lens, over)
import qualified Data.Map.Strict as M

import           Physics         (Angle (Angle), Position, Spin (Spin),
                                  Velocity, createV)

defaultWidth :: Int
defaultWidth = 800

defaultHeight :: Int
defaultHeight = 800

data GameState = GameState {
  gameWidth     :: Int,
  gameHeight    :: Int,
  availableId   :: Int,
  entityTypes   :: M.Map Int EntityType,
  positions     :: M.Map Int Position,
  velocities    :: M.Map Int Velocity,
  angles        :: M.Map Int Angle,
  spin          :: M.Map Int Spin,
  mousePosition :: Position,
  accelerating  :: Bool
} deriving (Eq, Show)

data EntityType = Player | Asteroid
  deriving (Eq, Show)

initGameState :: GameState
initGameState = let
                  empty = GameState {
                    gameWidth       = defaultWidth,
                    gameHeight      = defaultHeight,
                    availableId     = 1,
                    entityTypes     = M.empty,
                    positions       = M.empty,
                    velocities      = M.empty,
                    angles          = M.empty,
                    spin            = M.empty,
                    mousePosition   = createV 0 0,
                    accelerating    = False
                  }

                  addP = addPlayer (createV 400 400) (createV 0 0) (Angle 0)
                  addA = addAsteroid (createV 100 100) (createV 0 0) (Angle 0) (Spin 1)
                in addA $ addP empty

gameStateGameWidthL :: Lens' GameState Int
gameStateGameWidthL = lens gameWidth (\gs gw -> gs { gameWidth = gw })

gameStateGameHeightL :: Lens' GameState Int
gameStateGameHeightL = lens gameWidth (\gs gw -> gs { gameWidth = gw })

gameStateEntityTypesL :: Lens' GameState (M.Map Int EntityType)
gameStateEntityTypesL = lens entityTypes (\gs ts -> gs { entityTypes = ts })

gameStatePositionsL :: Lens' GameState (M.Map Int Position)
gameStatePositionsL = lens positions (\gs ps -> gs { positions = ps })

gameStateVelocitiesL :: Lens' GameState (M.Map Int Velocity)
gameStateVelocitiesL = lens velocities (\gs vs -> gs { velocities = vs })

gameStateAnglesL :: Lens' GameState (M.Map Int Angle)
gameStateAnglesL = lens angles (\gs as -> gs { angles = as })

gameStateSpinL :: Lens' GameState (M.Map Int Spin)
gameStateSpinL = lens spin (\gs s -> gs { spin = s })

gameStateMousePositionL :: Lens' GameState Position
gameStateMousePositionL = lens mousePosition (\gs mp -> gs { mousePosition = mp })

gameStateAcceleratingL :: Lens' GameState Bool
gameStateAcceleratingL = lens accelerating (\gs a -> gs { accelerating = a })

getNewId :: GameState -> (Int, GameState)
getNewId gs@GameState { availableId = aid } = (aid, gs { availableId = aid + 1})

addPlayer :: Position -> Velocity -> Angle -> GameState -> GameState
addPlayer pos vel angle = addEntity Player pos vel angle Nothing

addAsteroid :: Position -> Velocity -> Angle -> Spin -> GameState -> GameState
addAsteroid pos vel angle spi = addEntity Asteroid pos vel angle (Just spi)

addEntity :: EntityType -> Position -> Velocity -> Angle -> Maybe Spin -> GameState -> GameState
addEntity typ pos vel ang mspin gs = let
                                 (eid, gs') = getNewId gs
                                 setTyp = over gameStateEntityTypesL (M.insert eid typ)
                                 setPos = over gameStatePositionsL (M.insert eid pos)
                                 setVel = over gameStateVelocitiesL (M.insert eid vel)
                                 setAng = over gameStateAnglesL (M.insert eid ang)
                                 setSpi = maybe id (over gameStateSpinL . M.insert eid) mspin
                               in setTyp $ setPos $ setVel $ setAng $ setSpi gs'

destroyEntity :: Int -> GameState -> GameState
destroyEntity eid = let
                      destroyPos = over gameStatePositionsL (M.delete eid)
                      destroyVel = over gameStateVelocitiesL (M.delete eid)
                      destroyAng = over gameStateAnglesL (M.delete eid)
                    in foldr (.) id [destroyPos, destroyVel, destroyAng]
