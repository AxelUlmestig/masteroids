module Systems.AcceleratePlayer (acceleratePlayer) where

import           Control.Lens    (imap, over, view)
import qualified Data.Map.Strict as M

import qualified Constants
import           GameState       (EntityType (Player), GameState, accelerating,
                                  gameStateAnglesL, gameStateEntityTypesL,
                                  gameStateVelocitiesL)
import           Physics         (applyAcceleration, rotateV)

acceleratePlayer :: GameState -> GameState
acceleratePlayer gs = if not (accelerating gs)
                      then gs
                      else
                        foldr ($) gs
                        $ fmap (over gameStateVelocitiesL)
                        $ imap (flip M.adjust)
                        $ fmap (applyAcceleration . flip rotateV Constants.playerAcceleration)
                        $ M.intersectionWith const (view gameStateAnglesL gs)
                        $ M.filter (==Player)
                        $ view gameStateEntityTypesL gs
