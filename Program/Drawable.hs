{-# LANGUAGE FlexibleInstances #-}
module Program.Drawable where

import Graphics.Gloss.Data.Picture

import Util.ToFloat

import qualified Graphics.Gloss.Data.Color as G

import qualified Data.Types as D
import qualified Data.BallBox as B
import qualified Program.State as P

-- |Converts custom color type to Gloss color for drawing
toGlossColor :: D.Color -> G.Color
toGlossColor  D.Red                   = G.red
toGlossColor  D.Green                 = G.green
toGlossColor  D.Blue                  = G.blue
toGlossColor  D.Black                 = G.black
toGlossColor  D.White                 = G.white
toGlossColor (D.Color r g b)          = G.makeColor r g b 1.0
toGlossColor (D.AlphaColor r g b a)   = G.makeColor r g b a

-- |Drawable class to allow drawing of types with
--  the draw functions
class Drawable a where
    draw :: a -> Picture

    drawAt :: Point -> a -> Picture
    drawAt (x,y) = (Translate x y) . draw

-- |Draws the entire state of the program
instance Drawable P.State where
    draw state = Scale 1 (-1) $ Pictures $ zipWith drawAt slots (P.boxes state)
      where slots = map (\(x,y) -> (float x, float y) )
                  $ P.populationDrawSlots (P.viewSize state)
                                          (P.boxSize state)
                                          (P.boxSpace state)

-- |Draws a single ballbox
instance Drawable B.BallBox where
    draw box = Translate (width/2) (height/2)
             $ Pictures
             $ [rect] ++ circles
      where (width, height) = (\(a,b) -> (float a, float b)) $ B.size box
            rect = color G.white $ rectangleWire width height
            circles = map (\ball@(D.ColorBall (x,y) _ _) ->
                              drawAt (float x, float y) ball )
                          (B.balls box)

-- |Draws a single colorball
instance Drawable D.ColorBall where
    draw (D.ColorBall _ rad col) =
        Color (toGlossColor col) $ circleSolid (float rad)
