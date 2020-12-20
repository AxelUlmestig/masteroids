module HandleInput (handleInput) where

import           Control.Lens                       (set)
import           Graphics.Gloss.Interface.Pure.Game (Event (EventKey, EventMotion, EventResize),
                                                     Key (MouseButton, SpecialKey),
                                                     KeyState (Down, Up),
                                                     MouseButton (LeftButton),
                                                     SpecialKey (KeySpace))

import           GameState                          (GameState, gameHeight,
                                                     gameStateAcceleratingL,
                                                     gameStateMousePositionL,
                                                     gameWidth)
import           Physics                            (createV)

-- Gloss puts the origin in the middle of the screen by default. The logic
-- thinks that it's in the bottom left. The EventMotion event is offset to
-- accomodate this
handleInput :: Event -> GameState -> GameState
handleInput (EventMotion (mx, my)) gs                       = set gameStateMousePositionL (createV mx' my') gs
                                                                where
                                                                  mx' = mx + fromIntegral (gameWidth gs) / 2
                                                                  my' = my + fromIntegral (gameHeight gs) / 2
handleInput (EventKey (SpecialKey KeySpace) Down _ _) gs    = set gameStateAcceleratingL True gs
handleInput (EventKey (SpecialKey KeySpace) Up _ _) gs      = set gameStateAcceleratingL False gs
handleInput (EventKey (MouseButton LeftButton) Down _ _) gs = gs -- TODO: fire laser
handleInput (EventResize (width, height)) gs                = gs { gameWidth = width, gameHeight = height }
handleInput _ gs                                            = gs
