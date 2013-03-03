module Data.Types
    ( Shape(..), Color(..), ColorBall )
where

import Graphics.Rendering.OpenGL
                    ( GLfloat )

import GL.Aliases   ( glfloat, setColor4, pointFl )
import GL.Draw      ( Point, Drawable(draw), drawLine, fillCircle )

-- |Type synonym used in BallBox
type ColorBall = (Shape Int, Color)

-- |Datatypes for shapes
data Shape a =
       Circle       { p1 :: Point a, radius :: a }
     | Line         { p1 :: Point a, p2 :: Point a }
     | Pixel        { p1 :: Point a }
  deriving (Show, Read, Eq)

-- |Datatype for color
data Color = Red
           | Green
           | Blue
           | Black
           | White
           | Color GLfloat GLfloat GLfloat
           | AlphaColor GLfloat GLfloat GLfloat GLfloat
  deriving (Show, Read, Eq)

-- |Declaration for drawable
instance (Integral a) => Drawable (Shape a)  where
    draw (Circle p r) = fillCircle (pointFl p) (glfloat r)
    draw (Line p q) = drawLine (pointFl p) (pointFl q)
    draw (Pixel _) = return ()

-- |Makes Color drawable so its easy to change color
instance Drawable Color where
    draw Black = setColor4 0 0 0 1
    draw White = setColor4 1 1 1 1
    draw Red   = setColor4 1 0 0 1
    draw Green = setColor4 0 1 0 1
    draw Blue  = setColor4 0 0 1 1
    draw (Color r g b) =
                 setColor4 r g b 1
    draw (AlphaColor r g b a) =
                 setColor4 r g b a
