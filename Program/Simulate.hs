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
score (w,h) cs = float (length areaCovered) / 10
               + 10*(sum $ map ( borderOverlap . (\(Circle (x,y) r) -> (x,y,r))) cs)
               - (3 * float (length cs))
  where
    borderOverlap (x,y,r) = lineOverlap (0,h) (abs $ 0 - x) y r
                          + lineOverlap (0,h) (abs $ w - x) y r
                          + lineOverlap (0,w) (abs $ 0 - y) x r
                          + lineOverlap (0,w) (abs $ h - y) x r
    areaCovered = foldl' (\xs (Circle (x,y) r) -> 
                    filter (\(px, py) -> (px-x)*(px-x) + (py-y)*(py-y) > r*r) xs) 
                  [(x,y) | y <- [0..h], x <- [0..w]] cs

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
    | otherwise         = abs $ min (offset'+over) bottom'
                              - max (offset'-over) top'
  where offset' = float offset
        radius' = float radius
        dist'   = float dist
        over    = radius' * sin (acos (dist'/radius')) 
        (top',bottom') = (float top, float bottom)


-- |Mates two boxes to produce one child
mate :: (Point Int, [ColorBall]) -> 
        (Point Int, [ColorBall]) -> IO (Point Int, [ColorBall])
mate (size1@(w,h), b1s) (size2, b2s)
    | size1 /= size2    = error "mate cannot deal with differnt sizes"  
    | otherwise         = do
        (selection, newLength) <- (\s -> (s, length (filter (==True) s))) 
                 <$> forM [1..length both] (\_ -> randomRIO (True,False))
        dead <- rioI (0,min newLength 3)
        deadCircles <- forM [1..dead] 
            (\i -> rioI (0,newLength-i))
        mutations <- rioI (0,3)
        choices <- add mutations
                 $ remove deadCircles
                 $ map (\(_,cball) -> cball) 
                 $ filter (\(keep,_) -> keep) 
                 $ zip selection both
        return (size1, nub choices)
  where both = b1s++b2s
        rioI = randomRIO :: (Int,Int) -> IO Int  
        rioF = randomRIO :: (GLfloat,GLfloat) -> IO GLfloat

        remove [] xs = xs
        remove (i:is) xs = remove is (del i xs)

        add :: Int -> [ColorBall] -> IO [ColorBall]
        add i xs = do
            new <- forM [1..i] (\_ -> do
                x <- rioI (0,w)
                y <- rioI (0,h) 
                r <- rioI (3,20)
                [cr,cg,cb] <- replicateM 3 $ rioF (0.5,1.0)
                return (Circle (x,y) r, AlphaColor  cr cg cb 0.5) )
            return (new ++ xs)

        del i xs = (\(a,b) -> a ++ tail b) $ splitAt i xs 
