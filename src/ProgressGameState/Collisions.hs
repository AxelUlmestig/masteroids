module ProgressGameState.Collisions (handleCollisions) where

import           Control.Lens    (at, over, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

import           GameState       (GameState, gameStatePositionsL,
                                  gameStateVelocitiesL)
import           Physics         (Position, absV, bounce, distance)

defaultRadius :: Float
defaultRadius = 50
defaultMass :: Float
defaultMass = 1

handleCollisions :: GameState -> GameState
handleCollisions gs = let
                        collisions = detectCollisions $ M.toList $ view gameStatePositionsL gs :: [(Int, Int)]
                        applyUpdates = foldr (.) id $ fmap handleCollision collisions :: GameState -> GameState
                      in applyUpdates gs

detectCollisions :: [(Int, Position)] -> [(Int, Int)]
detectCollisions ((i, p):ps) = let
                                 collided = map ((,) i . fst) $ filter ((<(defaultRadius * 2)) . absV . distance p . snd) ps
                               in collided ++ detectCollisions ps
detectCollisions _           = []

-- TODO: don't use default masses
handleCollision :: (Int, Int) -> GameState -> GameState
handleCollision (id1, id2) gs = let
                                  maybeUpdate = do
                                    p1 <- view (gameStatePositionsL . at id1) gs
                                    v1 <- view (gameStateVelocitiesL . at id1) gs
                                    let m1 = defaultMass

                                    p2 <- view (gameStatePositionsL . at id2) gs
                                    v2 <- view (gameStateVelocitiesL . at id2) gs
                                    let m2 = defaultMass

                                    let (v1', v2') = bounce (p1, v1, m1) (p2, v2, m2)
                                    return $ M.insert id1 v1' . M.insert id2 v2'

                                  update = fromMaybe id maybeUpdate
                                in over gameStateVelocitiesL update gs
