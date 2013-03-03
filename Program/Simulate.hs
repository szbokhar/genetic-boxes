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
    borderCovered = sum $ map (borderOverlap . (\(Circle (x,y) r) -> (x,y,r))) cs
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
mate (size1, balls1) (size2, balls2)
    | size1 /= size2    = error "Cannot accept boxes of different sizes"
    | otherwise         = do
        -- Set up all random numbers for mating process
        (selected, newLength) <- listAndLength <$> randomBoolList
        badCircles <- circlesToRemove newLength
        newCircles <- rioI (0,3)
        -- generate the new list using the old ones
        choices <- add newCircles
                 $ remove badCircles
                 $ choose selected circles
        return (size1, nub choices)
  where
    -- Returns a tuple of the list and its length
    listAndLength xs = (xs, length $ filter id xs)
    -- Generates a random list of true and false values
    randomBoolList = forM [1..length circles] (const $ randomRIO (True,False))
    -- Generates a random list of indicies to remove from a list
    circlesToRemove n = do
        b <- rioI (0,min n 3)
        forM [1..b] (\i -> rioI (0,n-i) )
    -- Concaenation of allCircles list of circles
    circles = balls1++balls2

    -- Removes the elements whos indicies are given in the first list.
    -- Indicies must be in range.
    remove [] xs = xs
    remove (i:is) xs = remove is (del i xs)
      where del j ys = (\(a,b) -> a ++ tail b) $ splitAt j ys

    -- Generate new circles and add them to the supplied list
    add :: Int -> [ColorBall] -> IO [ColorBall]
    add i xs = do
        new <- forM [1..i] (\_ -> do
            -- Random geometry for circle
            x <- rioI (0,fst size1)
            y <- rioI (0,snd size1)
            r <- rioI (3,20)
            -- Random color for circle
            [cr,cg,cb] <- replicateM 3 $ rioF (0.5,1.0)
            -- Random circle
            return (Circle (x,y) r, AlphaColor  cr cg cb 0.5) )
        -- Whole list
        return (new ++ xs)

    -- Select the elements in the second list based on the selection list
    choose [] _  = []
    choose _  [] = []
    choose (True:xs) (circle:ys)    = circle : choose xs ys
    choose (_:xs)    (_:ys)         = choose xs ys

    -- Random function aliases
    rioI = randomRIO :: (Int,Int) -> IO Int
    rioF = randomRIO :: (GLfloat,GLfloat) -> IO GLfloat
