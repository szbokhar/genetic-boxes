module Data.BallBox
    ( BallBox(..), makeBox, randomBox, info, fitness, mateBoxes, drawBoxMating )
where

import Control.Monad                ( forM_, forM, replicateM )
import Data.Ord                     ( comparing )
import Foreign.C.Types              ( CFloat(..) )
import Graphics.Rendering.OpenGL    ( GLfloat )
import System.Random                ( randomRIO )

import Data.Types                   ( Shape(..), Color(..), ColorBall )
import Data.Point                   ( Point )
import GL.Draw                      ( Drawable(..), drawRect, fillCircle
                                    , drawLine, fillTriangle )

import GL.Aliases                   ( glfloat, pointInt, pointFl, pointGlint )
import Program.Simulate             ( score, scoreParts, mate )


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
        -- Draw each circle
        mapM_ (\(ball@(Circle cp _), color) -> do
            draw color
            draw $ offset (pointInt p) ball
            draw White
            fillCircle (pointFl $ add (pointInt p) (pointInt cp)) 1 ) bs

        -- Draw box border. Hilight red if it is selected
        if sel then draw Red
               else draw White
        drawRect p' (pointGlint $ add p' d')
      where
        (p', d') = (pointGlint p,pointGlint d)      -- Cast points to GLint
        add (x1,y1) (x2,y2) = (x1+x2,y1+y2)         -- Add points

        -- Offset circle
        offset shift (Circle pos rad) = Circle (add pos shift) rad
        offset (_,_) _ = error "offset: Only intended for use with Circle"


-- |Make BallBox a member of ordering
instance Ord BallBox where
    compare = comparing fitness


-- |Function to draw box mating process
drawBoxMating :: Integral a => Point a -> BallBox -> BallBox -> [BallBox] -> IO ()
drawBoxMating (x,y) dad mom cs = do
    -- Draw parents
    drawAt (x,y) dad
    drawAt (x+120, y) mom
    -- Draw arrow
    draw White
    drawLine (280, y' + 50) (360, y' + 50)
    fillTriangle (320, y'+30) (360, y'+50) (320, y'+ 70)
    -- Draw new children
    forM_ (zip [0..] cs) (\(i,box) -> drawAt (x+380+120*i, y) box )
  where
    y' = glfloat y


-- |Convienence function for compactly making a BallBox
makeBox :: Int -> (Int,Int)
               -> [(Int,Int,Int,GLfloat,GLfloat,GLfloat,GLfloat)] -> BallBox
makeBox bid (w,h) circs =
    BallBox bid False (w,h)
    $ map (\(x,y,r,cr,cg,cb,ca) ->
        (Circle (x,y) r, AlphaColor cr cg cb ca) ) circs


-- |Generates a random BallBox in the IO monad
randomBox :: Int -> (Int,Int) -> Int -> IO BallBox
randomBox bid (w,h) n = do
    -- Generate random circles
    randomCircles <- forM [1..n]  (\_ -> do
        -- Random geomerty
        x <- rioI (0,w)
        y <- rioI (0,h)
        r <- rioI (5,20)
        -- Random light color
        [cr,cg,cb] <- replicateM 3 $ rioF (0.5,1.0)
        -- Make shorthand circle reperesentation
        return (x,y,r,CFloat cr,CFloat cg,CFloat cb,0.5) )

    -- Return newly made box
    return $ makeBox bid (w,h) randomCircles
  where
    -- Short random function aliases
    rioI = randomRIO :: (Int,Int) -> IO Int
    rioF = randomRIO :: (Float,Float) -> IO Float


-- |Generates a succinct output string for a BallBox
info :: BallBox -> String
info box@(BallBox bid _ dimentions bs) =
    "Id: " ++ show bid ++
    "\tS: " ++ show (fitness box) ++
    "\tO: " ++ show o ++
    "\tA: " ++ show a ++
    "\tL: " ++ show l ++
    "\tC: " ++ show c
  where Just (o, a, l, c) = parts
        parts = scoreParts dimentions (map fst bs)


-- |Evaluates the fitness of a BallBox
fitness :: BallBox -> Float
fitness (BallBox _ _ dim cs) = score dim $ map fst cs


-- |Mates two ball boxes to make a certain number of children
mateBoxes :: Int -> BallBox -> BallBox -> IO [BallBox]
mateBoxes n (BallBox _ _ s1 b1) (BallBox _ _ s2 b2) =
    forM [1..n] (\_ -> do
        -- Pass essential data to mate function
        (s, cs) <- mate (s1, b1) (s2, b2)
        return $ BallBox (-1) False s cs)   -- Make box with dummy id
