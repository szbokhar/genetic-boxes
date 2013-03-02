module Program.Simulate 
    ( score, mate )
where

import Control.Applicative ( (<$>) )
import Control.Monad    ( forM, replicateM )
import Data.List        ( nub, foldl' )
import Graphics.Rendering.OpenGL 
                        ( GLfloat )
import System.Random    ( randomRIO )  

import Data.Types       ( Shape(Circle), ColorBall, Color(..) ) 
import Data.Point       ( Point )
import GL.Aliases       ( float )

-- |Rates the score for the data in a BallBox (Size and Circles)
score :: Point Int -> [Shape Int] -> Float
score (_,_) [] = 200
score (w,h) cs = areaCovered / 10.0
               + borderCovered * 10.0 
               - numOfCircles * 3.0
  where
    -- Number of circles
    numOfCircles = float $ length cs
    -- Amount of border covered by circles
    borderCovered = (sum $ map (borderOverlap . (\(Circle (x,y) r) -> (x,y,r))) cs)
    -- Total area covered by circles
    areaCovered = float $ length 
                $ foldl' (\xs (Circle (x,y) r) -> 
                      filter (\(px, py) -> 
                          let dx=px-x; dy=py-y 
                          in dx*dx + dy*dy > r*r ) 
                      xs ) 
                  [(x,y) | y <- [0..h], x <- [0..w]] cs

    -- Computes the number of pixels a circle is overlapping the border
    borderOverlap (x,y,r) = lineOverlap (0,h) (abs $ 0 - x) y r
                          + lineOverlap (0,h) (abs $ w - x) y r
                          + lineOverlap (0,w) (abs $ 0 - y) x r
                          + lineOverlap (0,w) (abs $ h - y) x r

-- |Returns the length of the line segment that is overlapped with
--  a circle.
--  (top, bottom) - the height coordinates of the top and bottom 
--                  of the wall
--  dist          - the distance from the center of the circle to the wall
--  offset        - the distance from the center of the circle to the top
--                  of the wall
--  radius        - the radius of the circle
lineOverlap :: Point Int -> Int -> Int -> Int -> Float
lineOverlap (top,bottom) dist offset radius
    | dist > radius     = 0
    | otherwise         = abs (low - high)
  where 
    -- Positions of the line segment lying within the circle
    high = min (offset'+over) bottom'
    low  = max (offset'-over) top'
    -- Half of ray overlap
    over    = radius' * sin (acos (dist'/radius')) 
    -- Convert types
    offset' = float offset
    radius' = float radius
    dist'   = float dist
    (top',bottom') = (float top, float bottom)


-- |Mates two boxes to produce one child
mate :: (Point Int, [ColorBall]) -> (Point Int, [ColorBall]) 
     -> IO (Point Int, [ColorBall])
mate (size1@(w,h), b1s) (size2, b2s)
    | size1 /= size2    = error "mate cannot deal with differnt sizes"  
    | otherwise         = do
        -- Set up all random numbers for mating process
        (selection, newLength) <- listAndLength <$> randomBoolList
        badCircles <- circlesToRemove newLength
        newCircles <- rioI (0,3)
        -- generate the new list using the old ones
        choices <- add newCircles
                 $ remove badCircles
                 $ choose selection both
        return (size1, nub choices)
  where 
    listAndLength xs = (xs, length $ filter id xs)
    randomBoolList = forM [1..length both] (const $ randomRIO (True,False))
    circlesToRemove newLength = do
        b <- rioI (0,min newLength 3)
        forM [1..b] (\i -> rioI (0,newLength-i) )
    both = b1s++b2s

    remove [] xs = xs
    remove (i:is) xs = remove is (del i xs)
      where del j ys = (\(a,b) -> a ++ tail b) $ splitAt j ys 

    add :: Int -> [ColorBall] -> IO [ColorBall]
    add i xs = do
        new <- forM [1..i] (\_ -> do
            x <- rioI (0,w)
            y <- rioI (0,h) 
            r <- rioI (3,20)
            [cr,cg,cb] <- replicateM 3 $ rioF (0.5,1.0)
            return (Circle (x,y) r, AlphaColor  cr cg cb 0.5) )
        return (new ++ xs)

    choose [] _  = []
    choose _  [] = []
    choose (True:xs) (circle:ys)    = circle : choose xs ys
    choose (_:xs)    (_:ys)         = choose xs ys

    rioI = randomRIO :: (Int,Int) -> IO Int  
    rioF = randomRIO :: (GLfloat,GLfloat) -> IO GLfloat
