module Data.Types
    ( Color(..), ColorBall(..), Point )
where

import Graphics.Rendering.OpenGL
                    ( GLfloat )

-- |Type synonym for a 2-tuple
type Point a = (a,a)

-- |Type synonym used in BallBox
data ColorBall = ColorBall { position :: Point Int
                           , radius :: Int
                           , ballColor :: Color }
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
