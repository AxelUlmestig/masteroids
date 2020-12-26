module Systems.HandleCollisions (handleCollisions) where

import           Control.Lens    (at, over, view)
import           Data.List       (sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

import           GameState       (EntityType (Asteroid, Laser, Player),
                                  GameState, destroyEntity, gameHeight,
                                  gameStateEntityTypesL, gameStateMassesL,
                                  gameStatePositionsL, gameStateRadiiL,
                                  gameStateVelocitiesL, gameWidth)
import           Physics         (Position, Radius, absV, bounce, distance)

handleCollisions :: GameState -> GameState
handleCollisions gs = let
                        border = (fromIntegral (gameWidth gs), fromIntegral (gameHeight gs))
                        collisions = detectCollisions border $ M.toList $ M.intersectionWith (,) (view gameStatePositionsL gs) (view gameStateRadiiL gs) :: [(Int, Int)]
                        applyUpdates = foldr (.) id $ fmap (handleCollision border) collisions :: GameState -> GameState
                      in applyUpdates gs

detectCollisions :: (Float, Float) -> [(Int, (Position, Radius))] -> [(Int, Int)]
detectCollisions border ((i, (p, r)):ps) = let
                                        touches (p2, r2) = absV (distance border p p2) < r + r2
                                        collided = map ((,) i . fst) $ filter (touches . snd) ps
                                      in collided ++ detectCollisions border ps
detectCollisions _ _                = []

handleCollision :: (Float, Float) -> (Int, Int) -> GameState -> GameState
handleCollision border (id1, id2) gs = fromMaybe gs $ do
                                         t1 <- view (gameStateEntityTypesL . at id1) gs
                                         t2 <- view (gameStateEntityTypesL . at id2) gs
                                         return $ f border (id1, t1) (id2, t2) gs

f :: (Float, Float) -> (Int, EntityType) -> (Int, EntityType) -> GameState -> GameState
f border x1 x2 = foldr (.) id $ case sortOn snd [x1, x2] of
                                  [(id1, Player), (id2, Asteroid)]  -> [bounceEntities border (id1, id2)]
                                  [(id1, Player), (id2, Laser)]     -> [destroyEntity id2, bounceEntities border (id1, id2)]
                                  [(id1, Asteroid), (id2, Laser)]   -> [destroyEntity id2, bounceEntities border (id1, id2)]
                                  _                                 -> []


bounceEntities :: (Float, Float) -> (Int, Int) -> GameState -> GameState
bounceEntities border (id1, id2) gs = let
                                         maybeUpdate = do
                                           p1 <- view (gameStatePositionsL . at id1) gs
                                           v1 <- view (gameStateVelocitiesL . at id1) gs
                                           m1 <- view (gameStateMassesL . at id1) gs

                                           p2 <- view (gameStatePositionsL . at id2) gs
                                           v2 <- view (gameStateVelocitiesL . at id2) gs
                                           m2 <- view (gameStateMassesL . at id2) gs

                                           let (v1', v2') = bounce border (p1, v1, m1) (p2, v2, m2)
                                           return $ M.insert id1 v1' . M.insert id2 v2'

                                         update = fromMaybe id maybeUpdate
                                       in over gameStateVelocitiesL update gs
