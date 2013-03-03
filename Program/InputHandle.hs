module Program.InputHandle where

import Control.Applicative ( (<$>) )
import Control.Monad    ( unless, forM )
import Data.Maybe       ( isNothing, fromJust )
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.BallBox     ( BallBox(..), info, fitness )
import Data.Point       ( Point )
import GL.Aliases       ( int, float, readInt )

import qualified Program.State as P

-- |Callback for regular character keyboard actions
keyboardChar :: P.State -> Char -> Position -> IO ()

-- Quit application
keyboardChar state 'q' (Position x y) = keyboardChar state 'Q' (Position x y)
keyboardChar state 'Q' _ = P.close state $= True

-- Display population
keyboardChar state 'p' _ = P.drawMode state $= P.Population

-- Rank population
keyboardChar state 'r' _ = P.rankPopulation state

-- Select population
keyboardChar state 's' _ = P.selectPopulation state 10

-- Mate population
keyboardChar state 'm' _ = P.matePopulation state

-- Add of remove one member form the pipulation
keyboardChar state '=' _ = P.increasePopulation state 1
keyboardChar state '-' _ = do
    bs <- get $ P.boxes state
    unless (null bs) (P.boxes state $= tail bs)

-- Display general info about the whole population
keyboardChar state 'd' _ = do
    bs <- get $ P.boxes state
    putStrLn $ "Average Fitness is "
            ++ show (sum (map fitness bs) / float (length bs))
    P.prompt state $= True

-- Start auto mode
keyboardChar state 'a' _ = do
    putStrLn "Enter the number of iterations you want to run"
    ln <- readInt <$> filter (/='\n') <$> getLine
    if (isNothing ln)
        then (putStrLn "You must enter the number of cycles")
        else (P.drawMode state $= P.Automate (fromJust ln) P.autoTimestep P.Display)
    return ()

-- Display help
keyboardChar state 'h' _ = do
    putStrLn "------\n\
             \Commands:\n\
             \    h - Show commands list\n\
             \    p - Switch to population mode\n\
             \    m - Switch to mate mode\n\
             \    r - Rank guys in population\n\
             \    d - Show overall stats\n\
             \    s - Select first n in the list and drop the rest\n\
             \    a - Automate for a given amount of cycles"
    P.prompt state $= True

-- Catch all
keyboardChar _ key (Position x y) = return ()


-- |Callback for keyboard key up actions
keyboardCharUp :: P.State -> Char -> Position -> IO ()
keyboardCharUp _ _ _ = return ()


-- |Callback for mouse click events
mouse :: P.State -> MouseButton -> KeyState -> Position -> IO ()
mouse _ _ _ _ = return ()


-- |Callback for mouse motion
mouseMotion :: P.State -> Position -> IO ()
mouseMotion _ _ = return ()


-- |Callback for passive mouse motion
passiveMotion :: P.State -> Position -> IO ()
passiveMotion state mousePos = do
    -- Get width, height and boxes with their positions
    width <- int <$> get (P.width state)
    height <- int <$> get (P.height state)
    boxes <- get (P.boxes state)
    -- Find out which pox (if any) the mouse if hovering over
    bs' <- forM (zip (positions (width,height)) boxes)
           (\(boxPosition, box@(BallBox _ selected boxSize _) ) ->
                -- If mouse over box
                if mouseOverBox boxPosition boxSize then
                    do unless selected $ do     -- If the box wasnt selected
                           print (info box)     -- before, the output the status
                           P.prompt state $= True
                       return box { selected = True }   -- Mark box as selected
                else   return box { selected = False } )-- Unmark
    -- Update boxes list
    P.boxes state $= bs'
  where
    (x,y) = (\(Position x' y') -> (int x', int y')) mousePos
    mouseOverBox (x1,y1) (w,h) = and [x > x1, y > y1, x < x1+w, y < y1+h]

-- |Positions to place boxes at on screen
positions :: (Enum a, Num a) => Point a-> [Point a]
positions (w,h) = [(x,y) | y <- [20,140..h-100], x <- [20,140..w-100] ]
