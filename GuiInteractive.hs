module Main ( main ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Environment   ( getArgs )

import GL.Bindings          ( display, idle, reshape )
import Program.EventHandle  ( timerLoop, keyboardChar, mouse, mouseMotion
                            , passiveMotion, keyboardCharUp )

import qualified Program.State as P

-- |Main program that sets up the glut window and
--  some basic callbacks. Also creates the program
--  state to be shared between all the callbacks.
main :: IO ()
main = do
    -- Initial state
    arguments <- getArgs
    state <- P.initializeState arguments


    -- Set up GLUT window
    (_,_) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 640 640
    _ <- createWindow "Genetic Algorithm"
    windowPosition $= Position 0 480

    blend $= Enabled
    blendEquation $= FuncAdd
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

    -- Set up callbacks
    displayCallback         $= display state
    idleCallback            $= Just (idle state)
    reshapeCallback         $= Just (reshape state)
    keyboardCallback        $= Just (keyboardChar state)
    keyboardUpCallback      $= Just (keyboardCharUp state)
    mouseCallback           $= Just (mouse state)
    motionCallback          $= Just (mouseMotion state)
    passiveMotionCallback   $= Just (passiveMotion state)

    -- Set up main control loop
    addTimerCallback (P.msPerFrame state) (timerLoop state)

    -- Start the program
    mainLoop
