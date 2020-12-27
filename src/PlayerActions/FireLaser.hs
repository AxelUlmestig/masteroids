module PlayerActions.FireLaser (fireLaser) where

import           Control.Lens    (at, over, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

import qualified Constants
import           GameState       (EntityType (Player), GameState, addLaser,
                                  gameStateAnglesL, gameStateEntityTypesL,
                                  gameStatePositionsL, gameStateRadiiL,
                                  gameStateVelocitiesL)
import           Physics         (addV, createV, movePosition, rotateV, scaleV,
                                  subtractV)


fireLaser :: GameState -> GameState
fireLaser gs = foldr ($) gs (fireFromPlayer <$> players gs)

fireFromPlayer :: Int -> GameState -> GameState
fireFromPlayer pid gs =
  fromMaybe gs $ do
    (pos, vel, ang, rad) <- (,,,) <$> view (gameStatePositionsL . at pid) gs
                                  <*> view (gameStateVelocitiesL . at pid) gs
                                  <*> view (gameStateAnglesL . at pid) gs
                                  <*> view (gameStateRadiiL . at pid) gs

    let laserPos = movePosition (rotateV ang (createV (rad + Constants.laserRadius + 1) 0)) pos
    let laserVel = addV (rotateV ang Constants.laserBaseSpeed) vel

    let playerMomentumDiff = scaleV (Constants.laserMass / Constants.playerMass) laserVel
    let recoilPlayer = over gameStateVelocitiesL $ M.adjust (subtractV playerMomentumDiff) pid

    return $ recoilPlayer $ addLaser laserPos laserVel ang gs

players :: GameState -> [Int]
players = fmap fst . filter ((==Player) . snd) . M.toList . view gameStateEntityTypesL
