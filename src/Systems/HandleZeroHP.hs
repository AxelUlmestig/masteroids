module Systems.HandleZeroHP (handleZeroHP) where

import           Control.Lens    (view)
import qualified Data.Map.Strict as M

import           GameState       (GameState, destroyEntity, gameStateHPL)

handleZeroHP :: GameState -> GameState
handleZeroHP gs = foldr ($) gs . fmap destroyEntity . M.keys . M.filter (<=0) . view gameStateHPL $ gs
