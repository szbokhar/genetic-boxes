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


-- |Draws the entire state of the program by drawing each of the BallBoxes
--  in each of the possible slots, and hilighting the one that is hovered with
--  the mouse
instance Drawable P.State where

    draw state = Scale 1 (-1)
               $ Pictures
               $ zipWith3 drawBallBox selectedList slots (P.boxes state)
      where
        -- Computes the positions of all the draw slots based on the
        -- window size, box size and box spacing
        slots = map (\(x,y) -> (float x, float y) )
              $ P.populationDrawSlots (P.viewSize state)
                                      (P.boxSize state)
                                      (P.boxSpace state)

        -- List that indicates which BallBoxes are selected
        selectedList = [ maybe False (\x -> i == x) (P.hoverSlot state)
                       | i <- [0..]]


-- |Draws a single Ballbox. Draws the border as red if the box is selected
instance Drawable B.BallBox where

    draw box = Translate (width/2) (height/2)   -- Sets Position of box
             $ Pictures
             $ [rect] ++ circles                -- Draw box and circles
      where
        -- Floating dimentions
        (width, height) = (\(a,b) -> (float a, float b)) $ B.size box

        -- Rectangle to draw with appropriate color
        rect = color (if B.selected box then G.red else G.white)
             $ rectangleWire width height

        -- All circles to draw
        circles = map (\ball@(D.ColorBall (x,y) _ _) ->
                          drawAt (float x, float y) ball )
                      (B.balls box)


-- |Draws a BallBox while accepting another paramater that indicates whether
--  the box is selected.
drawBallBox :: Bool -> Point -> B.BallBox -> Picture
drawBallBox selected position box = drawAt position (box { B.selected = selected })

-- |Draws a single colorball
instance Drawable D.ColorBall where

    draw (D.ColorBall _ rad col) =
        Color (toGlossColor col) $ circleSolid (float rad)
