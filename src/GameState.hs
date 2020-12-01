module GameState (
  GameState(..),
  EntityType(..),
  Position(..),
  Velocity(..),
  Angle(..),
  initGameState,
  gameStateGameWidthL,
  gameStateGameHeightL,
  gameStateEntityTypesL,
  gameStatePositionsL,
  gameStateVelocitiesL,
  gameStateAnglesL,
  gameStateMousePositionL,
  gameStateAcceleratingL,
  defaultWidth,
  defaultHeight,
  addEntity,
  destroyEntity
) where

import           Control.Lens    (Lens', lens, over)
import qualified Data.Map.Strict as M

import           Newtypes        (Angle (Angle), Position (Position),
                                  Velocity (Velocity))
import           Vector          (Vector (..))

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
                    mousePosition   = Position (Vector 0 0),
                    accelerating    = False
                  }

                  addPlayer = addEntity Player (Position (Vector 0 0)) (Velocity (Vector 0 0)) (Angle 0)
                  addAsteroid = addEntity Asteroid (Position (Vector 100 100)) (Velocity (Vector 6 4)) (Angle 0)
                in addAsteroid $ addPlayer empty

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

gameStateMousePositionL :: Lens' GameState Position
gameStateMousePositionL = lens mousePosition (\gs mp -> gs { mousePosition = mp })

gameStateAcceleratingL :: Lens' GameState Bool
gameStateAcceleratingL = lens accelerating (\gs a -> gs { accelerating = a })

getNewId :: GameState -> (Int, GameState)
getNewId gs@GameState { availableId = aid } = (aid, gs { availableId = aid + 1})

addEntity :: EntityType -> Position -> Velocity -> Angle -> GameState -> GameState
addEntity typ pos vel ang gs = let
                                 (eid, gs') = getNewId gs
                                 setTyp = over gameStateEntityTypesL (M.insert eid typ)
                                 setPos = over gameStatePositionsL (M.insert eid pos)
                                 setVel = over gameStateVelocitiesL (M.insert eid vel)
                                 setAng = over gameStateAnglesL (M.insert eid ang)
                               in setTyp $ setPos $ setVel $ setAng gs'

destroyEntity :: Int -> GameState -> GameState
destroyEntity eid = let
                      destroyPos = over gameStatePositionsL (M.delete eid)
                      destroyVel = over gameStateVelocitiesL (M.delete eid)
                      destroyAng = over gameStateAnglesL (M.delete eid)
                    in foldr (.) id [destroyPos, destroyVel, destroyAng]
