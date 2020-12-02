module HandleInput (handleInput) where

import           Control.Lens                       (set)
import           Graphics.Gloss.Interface.Pure.Game (Event (EventKey, EventMotion, EventResize),
                                                     Key (SpecialKey),
                                                     KeyState (Down, Up),
                                                     SpecialKey (KeySpace))

import           GameState                          (GameState, gameHeight,
                                                     gameStateAcceleratingL,
                                                     gameStateMousePositionL,
                                                     gameWidth)
import           Physics                            (createV)

handleInput :: Event -> GameState -> GameState
handleInput (EventMotion (mx, my)) gs                    = set gameStateMousePositionL (createV mx my) gs
handleInput (EventKey (SpecialKey KeySpace) Down _ _) gs = set gameStateAcceleratingL True gs
handleInput (EventKey (SpecialKey KeySpace) Up _ _) gs   = set gameStateAcceleratingL False gs
handleInput (EventResize (width, height)) gs             = gs { gameWidth = width, gameHeight = height }
handleInput _ gs                                         = gs
