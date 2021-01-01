module PlayerActions.FireLaser (fireLaser) where

import           Control.Lens                (at, over, view)
import           Control.Monad.Writer.Strict (Writer, writer)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromMaybe)

import qualified Constants
import           GameState                   (EntityType (Player), GameState,
                                              addLaser, gameStateAnglesL,
                                              gameStateEntityTypesL,
                                              gameStatePositionsL,
                                              gameStateRadiiL,
                                              gameStateVelocitiesL)
import           Physics                     (addV, createV, movePosition,
                                              rotateV, scaleV, subtractV)
import           SoundEffects                (SoundEffect (FireLaserSound))


fireLaser :: GameState -> Writer [SoundEffect] GameState
fireLaser gs = foldr (=<<) (pure gs) (fireFromPlayer <$> players gs)

fireFromPlayer :: Int -> GameState -> Writer [SoundEffect] GameState
fireFromPlayer pid gs =
  fromMaybe (pure gs) $ do
    (pos, vel, ang, rad) <- (,,,) <$> view (gameStatePositionsL . at pid) gs
                                  <*> view (gameStateVelocitiesL . at pid) gs
                                  <*> view (gameStateAnglesL . at pid) gs
                                  <*> view (gameStateRadiiL . at pid) gs

    let laserPos = movePosition (rotateV ang (createV (rad + Constants.laserRadius + 1) 0)) pos
    let laserVel = addV (rotateV ang Constants.laserBaseSpeed) vel

    let playerMomentumDiff = scaleV (Constants.laserMass / Constants.playerMass) laserVel
    let recoilPlayer = over gameStateVelocitiesL $ M.adjust (subtractV playerMomentumDiff) pid

    let gs' = recoilPlayer $ addLaser laserPos laserVel ang gs
    pure $ writer (gs', [FireLaserSound])

players :: GameState -> [Int]
players = fmap fst . filter ((==Player) . snd) . M.toList . view gameStateEntityTypesL
