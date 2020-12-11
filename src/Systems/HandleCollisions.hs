module Systems.HandleCollisions (handleCollisions) where

import           Control.Lens    (at, over, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

import           GameState       (GameState, gameHeight, gameStatePositionsL,
                                  gameStateVelocitiesL, gameWidth)
import           Physics         (Position, absV, bounce, distance)

defaultRadius :: Float
defaultRadius = 50
defaultMass :: Float
defaultMass = 1

handleCollisions :: GameState -> GameState
handleCollisions gs = let
                        border = (fromIntegral (gameWidth gs), fromIntegral (gameHeight gs))
                        collisions = detectCollisions border $ M.toList $ view gameStatePositionsL gs :: [(Int, Int)]
                        applyUpdates = foldr (.) id $ fmap (handleCollision border) collisions :: GameState -> GameState
                      in applyUpdates gs

detectCollisions :: (Float, Float) -> [(Int, Position)] -> [(Int, Int)]
detectCollisions border ((i, p):ps) = let
                                        collided = map ((,) i . fst) $ filter ((<(defaultRadius * 2)) . absV . distance border p . snd) ps
                                      in collided ++ detectCollisions border ps
detectCollisions _ _                = []

-- TODO: don't use default masses
handleCollision :: (Float, Float) -> (Int, Int) -> GameState -> GameState
handleCollision border (id1, id2) gs = let
                                         maybeUpdate = do
                                           p1 <- view (gameStatePositionsL . at id1) gs
                                           v1 <- view (gameStateVelocitiesL . at id1) gs
                                           let m1 = defaultMass

                                           p2 <- view (gameStatePositionsL . at id2) gs
                                           v2 <- view (gameStateVelocitiesL . at id2) gs
                                           let m2 = defaultMass

                                           let (v1', v2') = bounce border (p1, v1, m1) (p2, v2, m2)
                                           return $ M.insert id1 v1' . M.insert id2 v2'

                                         update = fromMaybe id maybeUpdate
                                       in over gameStateVelocitiesL update gs
