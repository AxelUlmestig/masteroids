module Systems.ApplySpin (applySpin) where

import           Control.Lens    (imap, over, view)
import qualified Data.Map.Strict as M

import           GameState       (GameState, gameStateAnglesL, gameStateSpinL)
import           Physics         (spinAngle)

applySpin :: GameState -> GameState
applySpin gs = let
                 update = foldr (.) id . imap (flip M.adjust) . fmap spinAngle . view gameStateSpinL
               in over gameStateAnglesL (update gs) gs
