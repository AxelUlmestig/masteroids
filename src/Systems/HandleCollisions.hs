module Systems.HandleCollisions (handleCollisions) where

import           Control.Lens                       (at, view)
import           Data.List                          (sortOn)
import qualified Data.Map.Strict                    as M
import           Data.Maybe                         (fromMaybe)

import           GameState                          (EntityType (Asteroid, Laser, Player),
                                                     GameState, destroyEntity,
                                                     gameHeight,
                                                     gameStateEntityTypesL,
                                                     gameStatePositionsL,
                                                     gameStateRadiiL, gameWidth)
import           Physics                            (Position, Radius, absV,
                                                     distance)
import           Systems.Collisions.BounceEntities  (bounceEntities)
import           Systems.Collisions.TakeLaserDamage (takeLaserDamage)

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
                                  [(id1, Player), (id2, Asteroid)]    -> [bounceEntities border (id1, id2)]
                                  [(id1, Player), (id2, Laser)]       -> [takeLaserDamage id1, destroyEntity id2, bounceEntities border (id1, id2)]
                                  [(id1, Asteroid), (id2, Laser)]     -> [takeLaserDamage id1, destroyEntity id2, bounceEntities border (id1, id2)]
                                  [(id1, Asteroid), (id2, Asteroid)]  -> [bounceEntities border (id1, id2)]
                                  _                                   -> []
