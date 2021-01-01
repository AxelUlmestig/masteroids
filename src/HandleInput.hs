module HandleInput (handleInput) where

import           Control.Lens                       (set)
import           Control.Monad.Writer.Strict        (Writer)
import           Data.Maybe                         (fromMaybe)
import           Graphics.Gloss.Interface.Pure.Game (Event (EventKey, EventMotion, EventResize),
                                                     Key (MouseButton, SpecialKey),
                                                     KeyState (Down, Up),
                                                     MouseButton (LeftButton),
                                                     SpecialKey (KeySpace))

import           GameState                          (GameState, gameHeight,
                                                     gameStateAcceleratingL,
                                                     gameStateMousePositionL,
                                                     gameWidth)
import           Physics                            (Position, createV)
import           PlayerActions.FireLaser            (fireLaser)
import           SoundEffects                       (SoundEffect,
                                                     playSoundEffects)

data PlayerInput = StartAcceleration
                 | StopAcceleration
                 | FireLaser
                 | MoveMouse Position
                 | ResizeWindow Int Int

handleInput :: Event -> GameState -> IO GameState
handleInput e gs = fromMaybe (pure gs) $ do
                     let windowDimensions = (gameWidth gs, gameHeight gs)
                     playerInput <- translateEvent windowDimensions e
                     pure $ playSoundEffects $ performAction playerInput gs

-- Gloss puts the origin in the middle of the screen by default. The logic
-- thinks that it's in the bottom left. The EventMotion event is offset to
-- accomodate this
translateEvent :: (Int, Int) -> Event -> Maybe PlayerInput
translateEvent (gw, gh) (EventMotion (mx, my))                = Just $ MoveMouse $ createV mx' my'
                                                                  where
                                                                    mx' = mx + fromIntegral gw / 2
                                                                    my' = my + fromIntegral gh / 2
translateEvent _ (EventKey (SpecialKey KeySpace) Down _ _)    = Just StartAcceleration
translateEvent _ (EventKey (SpecialKey KeySpace) Up _ _)      = Just StopAcceleration
translateEvent _ (EventKey (MouseButton LeftButton) Down _ _) = Just FireLaser
translateEvent _ (EventResize (width, height))                = Just $ ResizeWindow width height
translateEvent _ _                                            = Nothing

performAction :: PlayerInput -> GameState -> Writer [SoundEffect] GameState
performAction StartAcceleration gs    = pure $ set gameStateAcceleratingL True gs
performAction StopAcceleration gs     = pure $ set gameStateAcceleratingL False gs
performAction FireLaser gs            = fireLaser gs
performAction (MoveMouse pos) gs      = pure $ set gameStateMousePositionL pos gs
performAction (ResizeWindow gw gh) gs = pure $ gs { gameWidth = gw, gameHeight = gh }
