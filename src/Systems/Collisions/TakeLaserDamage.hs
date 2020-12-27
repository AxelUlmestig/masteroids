module Systems.Collisions.TakeLaserDamage (takeLaserDamage) where

import           Control.Lens    (over)
import qualified Data.Map.Strict as M

import qualified Constants
import           GameState       (GameState, gameStateHPL)

takeLaserDamage :: Int -> GameState -> GameState
takeLaserDamage eid = over gameStateHPL (M.adjust (subtract Constants.laserDamage) eid)
