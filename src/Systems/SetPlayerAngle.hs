module Systems.SetPlayerAngle (setPlayerAngle) where

import           Control.Lens    (at, ix, set, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust)

import           GameState       (EntityType (Player), GameState (..),
                                  gameStateAnglesL, gameStateEntityTypesL,
                                  gameStatePositionsL)
import           Physics         (Position, calculateAngle)

setPlayerAngle :: GameState -> GameState
setPlayerAngle gs = let
                         mp = mousePosition gs :: Position

                         update :: Int -> GameState -> GameState
                         update pid = let
                                        -- pp will crash if the player position is missing
                                        pp = fromJust $ view (gameStatePositionsL . at pid) gs :: Position
                                      in set (gameStateAnglesL . ix pid) (calculateAngle pp mp)

                         setAngles = foldr (.) id $ update <$> players gs
                       in
                         setAngles gs

players :: GameState -> [Int]
players = fmap fst . filter ((==Player) . snd) . M.toList . view gameStateEntityTypesL
