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
keyboardChar state 'q' (Position x y) = keyboardChar state 'Q' (Position x y)
keyboardChar state 'p' _ = P.drawMode state $= P.Population
keyboardChar state 'r' _ = P.rankPopulation state
keyboardChar state 's' _ = P.selectPopulation state 10
keyboardChar state 'm' _ = P.matePopulation state 
keyboardChar state 'Q' _ = P.close state $= True

keyboardChar state '=' _ = P.increasePopulation state 1 
keyboardChar state '-' _ = do
    bs <- get $ P.boxes state
    unless (null bs) (P.boxes state $= tail bs)

keyboardChar state 'd' _ = do
    bs <- get $ P.boxes state
    putStrLn $ "Average Fitness is " 
            ++ show (sum (map fitness bs) / float (length bs))
    P.prompt state $= True

keyboardChar state 'a' _ = do 
    putStrLn "Enter the number of iterations you want to run"
    ln <- readInt <$> filter (/='\n') <$> getLine
    if (isNothing ln) 
        then (putStrLn "You must enter the number of cycles")
        else (P.drawMode state $= P.Automate (fromJust ln) P.autoTimestep P.Display)
    return ()
    
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
    width <- int <$> get (P.width state)
    height <- int <$> get (P.height state)
    bs <- zip (positions (width,height)) <$> get (P.boxes state)
    bs' <- forM bs
           (\( (x1,y1) , box@(BallBox _ sel (w,h) _) ) ->
            if and [x > x1, y > y1, x < x1+w, y < y1+h] then
                 do unless sel $ do
                        print $ info box
                        P.prompt state $= True
                    return box { selected = True }
             else
                    return box { selected = False })
    P.boxes state $= bs'
    return ()
  where (x,y) = (\(Position x' y') -> (int x', int y')) mousePos

-- |Positions to place boxes at on screen
positions :: (Enum a, Num a) => Point a-> [Point a]
positions (w,h) = [(x,y) | y <- [20,140..h-100], x <- [20,140..w-100] ]
