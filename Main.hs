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
    opts <- (parseArguments defaultOptions) <$> getArgs
    when (isJust $ parseError opts)
       $ putStrLn (fromJust $ parseError opts)

    let (width, height) = optSize opts

    playIO (InWindow "Boxes" (optSize opts) (0,0))
           black
           30
           (P.initializeState opts)
           (return . (drawAt (-(float width)/2,(float height)/2)) )
           handleEvent
           P.timeUpdate
