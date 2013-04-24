module Data.Types
    ( Shape(..), Color(..), ColorBall, Point )
where

import Graphics.Rendering.OpenGL
                    ( GLfloat )

-- |Type synonym for a 2-tuple
type Point a = (a,a)

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
           | Color Float Float Float
           | AlphaColor Float Float Float Float
  deriving (Show, Read, Eq)
