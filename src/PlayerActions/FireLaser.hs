module PlayerActions.FireLaser (fireLaser) where

import           Control.Lens    (at, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

import           GameState       (EntityType (Player), GameState, addLaser,
                                  gameStateAnglesL, gameStateEntityTypesL,
                                  gameStatePositionsL, gameStateRadiiL,
                                  gameStateVelocitiesL)
import           Physics         (Acceleration, Radius, applyAcceleration,
                                  createV, movePosition, rotateV)

laserBaseSpeed :: Acceleration
laserBaseSpeed = createV 20 0

laserRadius :: Radius
laserRadius = 5

fireLaser :: GameState -> GameState
fireLaser gs = foldr ($) gs (fireFromPlayer <$> players gs)

fireFromPlayer :: Int -> GameState -> GameState
fireFromPlayer pid gs = fromMaybe gs gs'
  where
    gs' = do
            (pos, vel, ang, rad) <- (,,,) <$> view (gameStatePositionsL . at pid) gs
                                          <*> view (gameStateVelocitiesL . at pid) gs
                                          <*> view (gameStateAnglesL . at pid) gs
                                          <*> view (gameStateRadiiL . at pid) gs

            let laserPos = movePosition (rotateV ang (createV (rad + laserRadius + 1) 0)) pos
            let laserVel = applyAcceleration (rotateV ang laserBaseSpeed) vel
            return $ addLaser laserPos laserVel ang gs

players :: GameState -> [Int]
players = fmap fst . filter ((==Player) . snd) . M.toList . view gameStateEntityTypesL
