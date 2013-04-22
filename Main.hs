module Main ( main ) where

import Control.Applicative                  ( (<$>) )
import Control.Monad                        ( when )
import Data.Maybe                           ( isJust, fromJust )
import Graphics.Gloss.Interface.IO.Game
import System.Environment                   ( getArgs )

import Program.EventHandle
import Program.ParseArguments
import qualified Program.State as P

-- |Main program that sets up the glut window and
--  some basic callbacks. Also creates the program
--  state to be shared between all the callbacks.
main :: IO ()
main = do
    opts <- (parseArguments defaultOptions) <$> getArgs
    when (isJust $ parseError opts)
       $ putStrLn (fromJust $ parseError opts)
    playIO (InWindow "Boxes" (0,0) (optSize opts))
           white
           30
           (P.initializeState opts)
           P.drawState
           handleEvent
           P.timeUpdate
