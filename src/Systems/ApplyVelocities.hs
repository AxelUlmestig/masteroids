module Systems.ApplyVelocities (applyVelocities) where

import           Control.Lens    (imap, over, view)
import qualified Data.Map.Strict as M

import           GameState       (GameState, gameStatePositionsL,
                                  gameStateVelocitiesL)
import           Physics         (applyVelocity)

applyVelocities :: GameState -> GameState
applyVelocities gs = let
                       update = foldr (.) id . imap (flip M.adjust) . fmap applyVelocity . view gameStateVelocitiesL
                     in over gameStatePositionsL (update gs) gs
