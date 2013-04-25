module Data.BallBox where

import Control.Monad                ( forM, replicateM )
import System.Random                ( randomRIO )

import Data.Types                   ( Color(..), ColorBall(..) )


-- |Datatype for a BallBox.
--  A Ballbox is a box with certain width and height containing
--  a number of circles with their center lying inside the box
data BallBox =
     BallBox { boxId    :: Int
             , selected :: Bool
             , size     :: (Int,Int)
             , balls    :: [ColorBall] }
  deriving (Show, Eq)

-- |Generates a random BallBox in the IO monad
randomBox :: Int -> (Int,Int) -> Int -> IO BallBox
randomBox bid (w,h) n = do
    -- Generate random circles
    randomCircles <- forM [1..n]  (\_ -> do
        -- Random geomerty
        x <- rioI (-div w 2,div w 2)
        y <- rioI (-div h 2,div h 2)
        r <- rioI (5,20)
        -- Random light color
        [cr,cg,cb] <- replicateM 3 $ rioF (0.5,1.0)
        -- Make shorthand circle reperesentation
        return (x,y,r,cr,cg,cb,0.5) )

    -- Return newly made box
    return $ makeBox bid (w,h) randomCircles
  where
    -- Short random function aliases
    rioI = randomRIO :: (Int,Int) -> IO Int
    rioF = randomRIO :: (Float,Float) -> IO Float

-- |Convienence function for compactly making a BallBox
makeBox :: Int -> (Int,Int)
               -> [(Int,Int,Int,Float,Float,Float,Float)] -> BallBox
makeBox bid (w,h) circs =
    BallBox bid False (w,h)
    $ map (\(x,y,r,cr,cg,cb,ca) ->
        (ColorBall (x,y) r (AlphaColor cr cg cb ca)) ) circs


{-- |Make BallBox a member of ordering
instance Ord BallBox where
    compare = comparing fitness


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
        return $ BallBox (-1) False s cs)   -- Make box with dummy id--}
