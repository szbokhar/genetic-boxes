module Program.EventHandle
    ( timerLoop, keyboardChar, mouse, mouseMotion, passiveMotion
    , keyboardCharUp )
where

import Control.Applicative  ( (<$>) )
import Control.Monad        ( when )
import Data.Maybe           ( fromJust, isNothing )
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit          ( exitSuccess )
import System.IO            ( hWaitForInput, hFlush, stdin, stdout )

import Data.BallBox         ( drawBoxMating, fitness )
import Data.Point           ( Point )
import GL.Draw              ( Drawable(drawAt) )
import GL.Aliases           ( readInt, float )
import Program.InputHandle
                            ( keyboardChar, keyboardCharUp, mouse, mouseMotion
                            , passiveMotion, positions )

import qualified Program.State as P

-- |Self calling callback that controls the flow of the program
timerLoop :: P.State -> IO ()
timerLoop state = do
    -- Exit if the close flag is set
    finished <- get $ P.close state
    when finished exitSuccess

    -- Set up drawables
    mode <- get $ P.drawMode state
    width <- get $ P.width state
    height <- get $ P.height state

    -- Manage the mode for automatic simulation
    case checkMode mode of
      Just (True, P.Display) -> return ()
      Just (True, P.Sort) -> P.rankPopulation state
      Just (True, P.Select) -> do
          P.selectPopulation state 10
          generations <- get $ P.gens state
          when (generations /= 0 && generations `mod` 20 == 0) $ do
              bs <- get $ P.boxes state
              putStrLn $ "Average Fitness is "
                      ++ show (sum (map fitness bs) / float (length bs))
              P.prompt state $= True
      Just (True, P.Shuffle) -> P.preMate state
      Just (True, P.Mate) -> P.matePopulation state
      _ -> return ()

    -- Draw depending on drawMode
    case drawMode mode of
      P.Population -> do        -- Just draw population
            boxes <- get $ P.boxes state
            P.drawList state $= zipWith drawAt (positions (width, height)) boxes
      P.Mating -> do            -- Draw result of mating process
            (boxes, half) <- listAndHalflength <$> get (P.boxes state)
            P.drawList state $= zipWith3
                (\pos (d,m) (k1,k2) -> drawBoxMating pos d m [k1,k2] )
                matingRows
                (P.pairup $ take half boxes)
                (P.pairup $ drop half boxes)
      _ -> return ()

    -- Update mode
    P.drawMode state $= updateMode mode

    -- Output status after number of generations

    -- User interaction
    processCommands state


    -- Make next call to this function
    addTimerCallback (div 1000 30) (timerLoop state)

  where listAndHalflength xs = (xs, div (length xs) 2)
        matingRows = [(20,x) | x <- [20,140..]] :: [Point Int]

        -- Decides actual drawmode based on state drawMode
        drawMode (P.Automate _ _ P.Mate)    = P.Mating
        drawMode (P.Automate _ _ _)         = P.Population
        drawMode x                          = x

        -- Performs action if in auto mode
        checkMode (P.Automate _ t phase) = Just (t == P.autoTimestep, phase)
        checkMode _                      = Nothing

        -- Updates drawMode if in auto mode
        updateMode (P.Automate 0 0 P.Mate)  = P.Population
        updateMode (P.Automate x 0 P.Mate)  =
            P.Automate (x-1) P.autoTimestep P.Display
        updateMode (P.Automate x 0 phase)   =
            P.Automate x P.autoTimestep (succ phase)
        updateMode (P.Automate x t phase)   = P.Automate x (t-1) phase
        updateMode x = x

-- |Process commands for typing input
processCommands :: P.State -> IO ()
processCommands state = do
    -- Print tick only if something had been printed after the previous tick
    prompt <- get $ P.prompt state
    when prompt $ do
        P.prompt state $= False
        putStr "> "
        hFlush stdout

    -- Poll for input and pass if there is none
    hasInput <- hWaitForInput stdin 1
    when hasInput $ do
        -- If there is input, execute the command
        line <- getLine
        execute (words line)
        P.prompt state $= True
  where execute ["exit"]    = P.close state $= True
        execute ["mate"]    = P.matePopulation state
        execute ["rank"]    = P.rankPopulation state
        execute ["add"]     = P.increasePopulation state 1
        execute ["reset"]   = do
            P.gens state $= 0
            P.boxes state $= []
        execute ["select",wn]
            | isNothing n   = putStrLn "select must be supplied with a number"
            | otherwise     = P.selectPopulation state (fromJust n)
          where n = readInt wn
        execute ["add",wn]
            | isNothing n   = putStrLn ("\"" ++ wn ++ "\" is not a number")
            | otherwise     = P.increasePopulation state (fromJust n)
          where n = readInt wn
        execute ["auto",wn]
            | isNothing n   = putStrLn ("\"" ++ wn ++ "\" is not a number")
            | otherwise     = P.drawMode state $=
                                P.Automate (fromJust n) P.autoTimestep P.Display
          where n = readInt wn
        execute (x:_)       = putStrLn $ "Error, command not recognized: " ++ x
        execute _           = return ()
