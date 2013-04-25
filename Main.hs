module Main ( main ) where

import Control.Applicative                  ( (<$>) )
import Control.Monad                        ( when )
import Data.Maybe                           ( isJust, fromJust )
import Graphics.Gloss.Interface.IO.Game
import System.Environment                   ( getArgs )

import Program.Drawable                     ( Drawable(..) )
import Program.EventHandle                  ( handleEvent )
import Program.ParseArguments               ( ProgramOptions(parseError, optSize)
                                            , parseArguments, defaultOptions )
import Util.ToFloat
import qualified Program.State as P

-- |Main program that sets up the glut window and
--  some basic callbacks. Also creates the program
--  state to be shared between all the callbacks.
main :: IO ()
main = do
    -- Parse commandline arguments into program options
    opts <- (parseArguments defaultOptions) <$> getArgs
    when (isJust $ parseError opts)
       $ putStrLn (fromJust $ parseError opts)

    -- Get width and height of window
    let (width, height) = (\(x,y) -> (float x, float y)) $ optSize opts

    -- Create window and setup drawing
    playIO (InWindow "Boxes" (optSize opts) (0,0))  -- Window setup
           black                                    -- Background color
           30                                       -- Frames per second
           (P.initializeState opts)                 -- Initial state
           (return . (drawAt (-width/2,height/2)) ) -- Draw state function
           handleEvent                              -- Handle input events
           P.timeUpdate                             -- Automaticaly step state
