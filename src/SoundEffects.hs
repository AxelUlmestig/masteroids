module SoundEffects (playSoundEffects, SoundEffect(..)) where

import           Control.Monad.Writer.Strict (Writer, runWriter)
import           Sound.ALUT                  (SoundDataSource (File),
                                              createBuffer, createContext,
                                              currentContext, genObjectNames,
                                              openDevice, play, queueBuffers,
                                              runALUTUsingCurrentContext,
                                              withProgNameAndArgs, ($=))

import           GameState                   (GameState)

data SoundEffect = FireLaserSound
                 deriving (Eq, Show)

playSoundEffects :: Writer [SoundEffect] GameState -> IO GameState
playSoundEffects x = do
                       let (gs, soundEffects) = runWriter x
                       mapM_ playSound soundEffects
                       pure gs

-- TODO: should the "devices" be opened in some central place?
playSound :: SoundEffect -> IO ()
playSound FireLaserSound = withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> do
                             (Just device) <- openDevice Nothing
                             (Just context) <- createContext device []
                             currentContext $= Just context
                             buffer1 <- createBuffer $ File "assets/sounds/shoot1.wav"
                             [source] <- genObjectNames 1
                             queueBuffers source [buffer1]
                             play [source]
                             return ()
