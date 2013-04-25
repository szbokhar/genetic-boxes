{-# LANGUAGE FlexibleInstances #-}
module Program.Drawable where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Data.Types       ( ColorBall(..), toGlossColor )
import Util.ToFloat

import qualified Data.BallBox as B
import qualified Program.State as P

class Drawable a where
    draw :: a -> Picture

    drawAt :: Point -> a -> Picture
    drawAt (x,y) = (Translate x y) . draw


instance Drawable P.State where
    draw state = Scale 1 (-1) $ Pictures $ zipWith drawAt slots (P.boxes state)
      where (winW,winH) = P.viewSize state
            (boxW, boxH) = P.boxSize state
            s = P.boxSpace state
            slots = [ (x, y)
                    | y <- map float [s, 2*s+boxH .. winH-boxH-s]
                    , x <- map float [s, 2*s+boxW .. winW-boxW-s] ]

instance Drawable B.BallBox where
    draw box = Translate (width/2) (height/2)
             $ Pictures
             $ [rect] ++ circles
      where (width, height) = (\(a,b) -> (float a, float b)) $ B.size box
            rect = color white $ rectangleWire width height
            circles = map (\ball@(ColorBall (x,y) _ _) -> drawAt (float x, float y) ball) (B.balls box)

instance Drawable ColorBall where
    draw (ColorBall pos rad col) = Color (toGlossColor col) $ circleSolid (float rad)
