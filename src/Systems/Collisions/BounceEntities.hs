module Systems.Collisions.BounceEntities (bounceEntities) where

import           Control.Lens    (at, over, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

import           GameState       (GameState, gameStateMassesL,
                                  gameStatePositionsL, gameStateVelocitiesL)
import           Physics         (bounce)

bounceEntities :: (Float, Float) -> (Int, Int) -> GameState -> GameState
bounceEntities border (id1, id2) gs =
  fromMaybe gs $ do
    p1 <- view (gameStatePositionsL . at id1) gs
    v1 <- view (gameStateVelocitiesL . at id1) gs
    m1 <- view (gameStateMassesL . at id1) gs

    p2 <- view (gameStatePositionsL . at id2) gs
    v2 <- view (gameStateVelocitiesL . at id2) gs
    m2 <- view (gameStateMassesL . at id2) gs

    let (v1', v2') = bounce border (p1, v1, m1) (p2, v2, m2)
    return $ over gameStateVelocitiesL (M.insert id1 v1' . M.insert id2 v2') gs
