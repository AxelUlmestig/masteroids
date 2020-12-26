module Main where

import           Graphics.Gloss    (Display (InWindow), black, play)

import           Assets            (loadAssets)
import qualified Constants
import           GameState         (initGameState)
import           HandleInput       (handleInput)
import           ProgressGameState (progressGameState)
import           Render            (render)

windowDisplay :: Display
windowDisplay = InWindow "Window" (Constants.defaultWidth, Constants.defaultHeight) (10, 10)

main :: IO ()
main = do
  assets <- loadAssets
  play
    windowDisplay
    black
    60
    initGameState
    (render assets)
    handleInput
    progressGameState
