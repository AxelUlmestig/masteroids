module Main where

import           Graphics.Gloss    (Display (InWindow), black, play)

import           Assets            (loadAssets)
import           GameState         (defaultHeight, defaultWidth, initGameState)
import           HandleInput       (handleInput)
import           ProgressGameState (progressGameState)
import           Render            (render)

windowDisplay :: Display
windowDisplay = InWindow "Window" (defaultWidth, defaultHeight) (10, 10)

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
