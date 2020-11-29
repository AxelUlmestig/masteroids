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
  gameStateMousePositionL,
  gameStateAcceleratingL,
  defaultWidth,
  defaultHeight,
  addEntity,
  destroyEntity
) where

import           Control.Lens    (Lens', lens, over)
import qualified Data.Map.Strict as M

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
  positions     :: M.Map Int (Vector Float),
  velocities    :: M.Map Int (Vector Float),
  angles        :: M.Map Int Float,
  mousePosition :: Vector Float,
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
                    mousePosition   = Vector 0 0,
                    accelerating    = False
                  }
                in addEntity Player (Vector 0 0) (Vector 0 0) 0 empty

gameStateGameWidthL :: Lens' GameState Int
gameStateGameWidthL = lens gameWidth (\gs gw -> gs { gameWidth = gw })

gameStateGameHeightL :: Lens' GameState Int
gameStateGameHeightL = lens gameWidth (\gs gw -> gs { gameWidth = gw })

gameStateEntityTypesL :: Lens' GameState (M.Map Int EntityType)
gameStateEntityTypesL = lens entityTypes (\gs ts -> gs { entityTypes = ts })

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

getNewId :: GameState -> (Int, GameState)
getNewId gs@GameState { availableId = aid } = (aid, gs { availableId = aid + 1})

addEntity :: EntityType -> Vector Float -> Vector Float -> Float -> GameState -> GameState
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
