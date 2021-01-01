module Main where

import           Graphics.Gloss                   (Display (InWindow), black)
import           Graphics.Gloss.Interface.IO.Game (playIO)

import           Assets                           (loadAssets)
import qualified Constants
import           GameState                        (initGameState)
import           HandleInput                      (handleInput)
import           ProgressGameState                (progressGameState)
import           Render                           (render)

windowDisplay :: Display
windowDisplay = InWindow "Window" (Constants.defaultWidth, Constants.defaultHeight) (10, 10)

main :: IO ()
main = do
  assets <- loadAssets
  playIO
    windowDisplay
    black
    60
    initGameState
    (pure . render assets)
    handleInput
    progressGameState
