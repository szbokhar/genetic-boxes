module Data.Types
    ( Color(..), ColorBall(..), Point, toGlossColor )
where

import qualified Graphics.Gloss.Data.Color as G

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

toGlossColor :: Color -> G.Color
toGlossColor  Red                   = G.red
toGlossColor  Green                 = G.green
toGlossColor  Blue                  = G.blue
toGlossColor  Black                 = G.black
toGlossColor  White                 = G.white
toGlossColor (Color r g b)          = G.makeColor r g b 1.0
toGlossColor (AlphaColor r g b a)   = G.makeColor r g b a

