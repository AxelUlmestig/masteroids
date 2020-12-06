module ProgressGameState.Collisions (handleCollisions) where

import           Control.Lens    (at, over, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust)

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

handleCollision :: (Int, Int) -> GameState -> GameState
handleCollision (id1, id2) gs = let
                                  p1 = fromJust $ view (gameStatePositionsL . at id1) gs
                                  p2 = fromJust $ view (gameStatePositionsL . at id2) gs
                                  v1 = fromJust $ view (gameStateVelocitiesL . at id1) gs
                                  v2 = fromJust $ view (gameStateVelocitiesL . at id2) gs
                                  (v1', v2') = bounce (p1, v1, defaultMass) (p2, v2, defaultMass) -- TODO: don't use default masses
                                  update = M.insert id1 v1' . M.insert id2 v2'
                                in over gameStateVelocitiesL update gs
