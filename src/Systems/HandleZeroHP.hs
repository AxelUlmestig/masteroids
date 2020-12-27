module Systems.HandleZeroHP (handleZeroHP) where

import           Control.Lens    (at, imap, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

import           GameState       (EntityType (Asteroid), GameState, addAsteroid,
                                  destroyEntity, gameStateEntityTypesL,
                                  gameStateHPL, gameStateMassesL,
                                  gameStatePositionsL, gameStateRadiiL,
                                  gameStateSpinL, gameStateVelocitiesL)
import           Physics         (Angle (Angle), createV, movePosition,
                                  normalizeV, rotateV, scaleV, toPair)



handleZeroHP :: GameState -> GameState
handleZeroHP gs = foldr ($) gs . imap kill . M.intersectionWith const (view gameStateEntityTypesL gs) . M.filter (<=0) . view gameStateHPL $ gs

kill :: Int -> EntityType -> GameState -> GameState
kill eid Asteroid = breakAsteroid eid
kill eid _        = destroyEntity eid

breakAsteroid :: Int -> GameState -> GameState
breakAsteroid eid gs =
  fromMaybe gs $ do
    typ <- view (gameStateEntityTypesL . at eid) gs

    if typ /= Asteroid then
      Nothing
    else do
      pos0  <- view (gameStatePositionsL . at eid) gs
      vel0  <- view (gameStateVelocitiesL . at eid) gs
      mass0 <- view (gameStateMassesL . at eid) gs
      rad0  <- view (gameStateRadiiL . at eid) gs
      spin0 <- view (gameStateSpinL . at eid) gs

      let (dx, dy) = toPair $ scaleV (sqrt 2 * rad0 + 1) $ rotateV (Angle 90) $ normalizeV vel0
      let pos1 = movePosition (createV dx dy) pos0
      let pos2 = movePosition (createV (-dx) (-dy)) pos0

      return
        $ addAsteroid pos1 vel0 (mass0 / 2) (rad0 / sqrt 2) spin0
        $ addAsteroid pos2 vel0 (mass0 / 2) (rad0 / sqrt 2) spin0
        $ destroyEntity eid
        gs
