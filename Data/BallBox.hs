module Data.BallBox
    ( BallBox(..), makeBox, randomBox, info, fitness, mateBoxes, drawBoxMating )
where

import Control.Monad    ( forM_, forM, replicateM )
import Data.Ord         ( comparing )
import Foreign.C.Types  ( CFloat(..) )
import Graphics.Rendering.OpenGL 
                        ( GLfloat )
import System.Random    ( randomRIO )

import Data.Types       ( Shape(..), Color(..), ColorBall ) 
import Data.Point       ( Point, Pt(..), fromPt )
import GL.Draw          ( Drawable(..), drawRect, fillCircle, drawLine, fillTriangle )
import GL.Aliases       ( glfloat, pointInt, pointFl, pointGlint )
import Program.Simulate ( score, mate )

-- |Datatype for a BallBox.
--  A Ballbox is a box with certain width and height containing 
--  a number of circles with their center lying inside the box
data BallBox = 
     BallBox { boxId :: Int
             , selected :: Bool
             , size :: (Int,Int)
             , balls :: [ColorBall] }
  deriving (Show, Eq)

-- |Makes the BallBox drawable
instance Drawable BallBox where
    drawAt p (BallBox _ sel d bs) = do
        mapM_ (\(ball@(Circle cp _), color) -> do
            draw color
            draw $ offset (pointInt p) ball
            draw White
            fillCircle (pointFl $ fromPt $ Pt (pointInt p) + Pt (pointInt cp)) 1
            ) bs
        if sel then draw Red
               else draw White
        drawRect (fromPt p') (fromPt (p'+d'))
      where (p', d') = (Pt $ pointGlint p, Pt $ pointGlint d)

-- |Make BallBox a member of ordering
instance Ord BallBox where
    compare = comparing fitness

-- |Function to draw box mating process
drawBoxMating :: Integral a => Point a -> BallBox -> BallBox -> [BallBox] -> IO ()
drawBoxMating (x,y) d m cs = do
    drawAt (x,y) d
    drawAt (x+120, y) m
    draw White
    drawLine (280, y' + 50) (360, y' + 50)
    fillTriangle (320, y' + 30) (360, y'+50) (320, y' + 70)
    forM_ (zip [0..] cs) (\(i,box) -> drawAt (x+380+120*i, y) box)
  where y' = glfloat y

-- |Convienence function for compactly making a BallBox
makeBox :: Int -> (Int,Int) 
               -> [(Int,Int,Int,GLfloat,GLfloat,GLfloat,GLfloat)] -> BallBox
makeBox bid (w,h) circs = 
    BallBox bid False (w,h) $ 
    map (\(x,y,r,cr,cg,cb,ca) -> (Circle (x,y) r, AlphaColor cr cg cb ca)) circs 

-- |Generates a random BallBox in the IO monad
randomBox :: Int -> (Int,Int) -> Int -> IO BallBox
randomBox bid (w,h) n = do
    com <- forM [1..n]  (\_ -> do
        x <- rioI (0,w)
        y <- rioI (0,h)
        r <- rioI (3,20)
        [cr,cg,cb] <- replicateM 3 $ rioF (0.5,1.0)
        return (x,y,r,CFloat cr,CFloat cg,CFloat cb,0.5))
    return $ makeBox bid (w,h) com
  where rioI = randomRIO :: (Int,Int) -> IO Int
        rioF = randomRIO :: (Float,Float) -> IO Float

-- |Generates a succinct output string for a BallBox
info :: BallBox -> String
info box@(BallBox bid _ _ bs) = 
    "BoxId: " ++ show bid ++ 
    "   Count: " ++ show (length bs) ++ 
    "   Score: " ++ show (fitness box)

-- |Evaluates the fitness of a BallBox
fitness :: BallBox -> Float
fitness (BallBox _ _ dim cs) = score dim $ map fst cs

-- |Mates two ball boxes to make a certain number of children
mateBoxes :: Int -> BallBox -> BallBox -> IO [BallBox]
mateBoxes n b1 b2 = forM [1..n] (\_ -> do
    (s, cs) <- mate (size b1, balls b1) (size b2, balls b2)
    return $ BallBox (-1) False s cs)



-- Utility functions
-- |Convienence function for offsetting a Circle shape
offset :: Num a => Point a -> Shape a -> Shape a
offset (dx, dy) (Circle (x,y) rad) = Circle (x+dx, y+dy) rad
offset (_,_) _ = error "offset: Only intended for use with Circle"
