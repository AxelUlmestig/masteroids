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
  gameStateRadiiL,
  gameStateMassesL,
  gameStateHPL,
  gameStateMousePositionL,
  gameStateAcceleratingL,

  addAsteroid,
  addLaser,
  destroyEntity
) where

import           Control.Applicative ((<|>))
import           Control.Lens        (Lens', lens, over)
import qualified Data.Map.Strict     as M

import qualified Constants
import           Physics             (Angle (Angle), Hitpoints, Mass, Position,
                                      Radius, Spin, Velocity, createV)

data GameState = GameState {
  gameWidth     :: Int,
  gameHeight    :: Int,
  availableId   :: Int,
  entityTypes   :: M.Map Int EntityType,
  positions     :: M.Map Int Position,
  velocities    :: M.Map Int Velocity,
  angles        :: M.Map Int Angle,
  spins         :: M.Map Int Spin,
  radii         :: M.Map Int Radius,
  masses        :: M.Map Int Mass,
  hitpoints     :: M.Map Int Hitpoints,
  mousePosition :: Position,
  accelerating  :: Bool
} deriving (Eq, Show)

data EntityType = Player
                | Asteroid
                | Laser
  deriving (Eq, Ord, Show)

initGameState :: GameState
initGameState = let
                  empty = GameState {
                    gameWidth       = Constants.defaultWidth,
                    gameHeight      = Constants.defaultHeight,
                    availableId     = 1,
                    entityTypes     = M.empty,
                    positions       = M.empty,
                    velocities      = M.empty,
                    angles          = M.empty,
                    spins           = M.empty,
                    radii           = M.empty,
                    masses          = M.empty,
                    hitpoints       = M.empty,
                    mousePosition   = createV 0 0,
                    accelerating    = False
                  }

                  addP = addPlayer (createV 400 400) (createV 0 0)
                  addA = addAsteroid (createV 100 100) (createV 0 0) Constants.asteroidDefaultSpin
                in addA $ addP empty

-- lenses
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
gameStateSpinL = lens spins (\gs s -> gs { spins = s })

gameStateRadiiL :: Lens' GameState (M.Map Int Radius)
gameStateRadiiL = lens radii (\gs r -> gs { radii = r })

gameStateMassesL :: Lens' GameState (M.Map Int Mass)
gameStateMassesL = lens masses (\gs m -> gs { masses = m })

gameStateHPL :: Lens' GameState (M.Map Int Hitpoints)
gameStateHPL = lens hitpoints (\gs hp -> gs { hitpoints = hp })

gameStateMousePositionL :: Lens' GameState Position
gameStateMousePositionL = lens mousePosition (\gs mp -> gs { mousePosition = mp })

gameStateAcceleratingL :: Lens' GameState Bool
gameStateAcceleratingL = lens accelerating (\gs a -> gs { accelerating = a })

-- entity management
data EntityData = EntityData
                    (Maybe Position)
                    (Maybe Velocity)
                    (Maybe Angle)
                    (Maybe Spin)
                    (Maybe Radius)
                    (Maybe Mass)
                    (Maybe Hitpoints)

instance Semigroup EntityData where
  (EntityData p1 v1 a1 s1 r1 m1 hp1) <> (EntityData p2 v2 a2 s2 r2 m2 hp2) =
    EntityData
      (p2 <|> p1)
      (v2 <|> v1)
      (a2 <|> a1)
      (s2 <|> s1)
      (r2 <|> r1)
      (m2 <|> m1)
      (hp2 <|> hp1)

instance Monoid EntityData where
  mempty = EntityData Nothing Nothing Nothing Nothing Nothing Nothing Nothing

withPosition :: Position -> EntityData
withPosition pos = EntityData (Just pos) Nothing Nothing Nothing Nothing Nothing Nothing

withVelocity :: Velocity -> EntityData
withVelocity vel = EntityData Nothing (Just vel) Nothing Nothing Nothing Nothing Nothing

withAngle :: Angle -> EntityData
withAngle ang = EntityData Nothing Nothing (Just ang) Nothing Nothing Nothing Nothing

withSpin :: Spin -> EntityData
withSpin spi = EntityData Nothing Nothing Nothing (Just spi) Nothing Nothing Nothing

withRadius :: Radius -> EntityData
withRadius rad = EntityData Nothing Nothing Nothing Nothing (Just rad) Nothing Nothing

withMass :: Mass -> EntityData
withMass mas = EntityData Nothing Nothing Nothing Nothing Nothing (Just mas) Nothing

withHitpoints :: Hitpoints -> EntityData
withHitpoints hp = EntityData Nothing Nothing Nothing Nothing Nothing Nothing (Just hp)

getNewId :: GameState -> (Int, GameState)
getNewId gs@GameState { availableId = aid } = (aid, gs { availableId = aid + 1})

addPlayer :: Position -> Velocity -> GameState -> GameState
addPlayer pos vel = addEntity Player $ withPosition pos
                                       <> withVelocity vel
                                       <> withAngle (Angle 0)
                                       <> withRadius Constants.playerRadius
                                       <> withMass Constants.playerMass
                                       <> withHitpoints Constants.playerMaxHP

addAsteroid :: Position -> Velocity -> Spin -> GameState -> GameState
addAsteroid pos vel spi = addEntity Asteroid $ withPosition pos
                                               <> withVelocity vel
                                               <> withAngle (Angle 0)
                                               <> withSpin spi
                                               <> withRadius Constants.asteroidRadius
                                               <> withMass Constants.asteroidMass
                                               <> withHitpoints 10

addLaser :: Position -> Velocity -> Angle -> GameState -> GameState
addLaser pos vel ang = addEntity Laser $ withPosition pos
                                       <> withVelocity vel
                                       <> withAngle ang
                                       <> withRadius Constants.laserRadius
                                       <> withMass Constants.laserMass

addEntity :: EntityType -> EntityData -> GameState -> GameState
addEntity typ (EntityData pos vel ang spi rad mas hp) gs = let
                                 (eid, gs') = getNewId gs
                                 setTyp = over gameStateEntityTypesL (M.insert eid typ)
                                 setPos = maybe id (over gameStatePositionsL . M.insert eid) pos
                                 setVel = maybe id (over gameStateVelocitiesL . M.insert eid) vel
                                 setAng = maybe id (over gameStateAnglesL . M.insert eid) ang
                                 setSpi = maybe id (over gameStateSpinL . M.insert eid) spi
                                 setRad = maybe id (over gameStateRadiiL . M.insert eid) rad
                                 setMas = maybe id (over gameStateMassesL . M.insert eid) mas
                                 setHP  = maybe id (over gameStateHPL . M.insert eid) hp
                               in setTyp $ setPos $ setVel $ setAng $ setSpi $ setRad $ setMas $ setHP gs'

destroyEntity :: Int -> GameState -> GameState
destroyEntity eid = let
                      destroyPos  = over gameStatePositionsL (M.delete eid)
                      destroyVel  = over gameStateVelocitiesL (M.delete eid)
                      destroyAng  = over gameStateAnglesL (M.delete eid)
                      destroySpin = over gameStateSpinL (M.delete eid)
                      destroyRad  = over gameStateRadiiL (M.delete eid)
                      destroyMas  = over gameStateMassesL (M.delete eid)
                      destroyHP  = over gameStateHPL (M.delete eid)
                    in foldr (.) id [destroyHP, destroyPos, destroyVel, destroyAng, destroySpin, destroyRad, destroyMas]
