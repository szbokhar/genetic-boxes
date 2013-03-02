{-# LANGUAGE NoMonomorphismRestriction #-}

module GL.Draw
    ( Point, Drawable(..)
    , drawPoint, drawLine, drawRect, fillRect, drawCircle, fillCircle
    , drawTriangle, fillTriangle )
where

import Graphics.Rendering.OpenGL

import Data.Point

-- |Class definition for types that can be drawn
class Drawable a where
    draw :: a -> IO ()
    draw = drawAt (0 :: Int, 0 :: Int)

    drawAt :: Integral n => Point n -> a -> IO ()
    drawAt _ = draw

-- |Function that draws a single pixel
drawPoint :: (Enum a, Floating a, VertexComponent a) => Point a -> IO ()
drawPoint (x,y) = fillCircle (x,y) 1

-- |Function that draws a line between two points
drawLine :: (Num a, VertexComponent a) => Point a -> Point a -> IO ()
drawLine p1 p2 = renderPrimitive Lines
    $ mapM_ (\(x,y) -> vertex $ Vertex3 x y 0 )
    $ lineV p1 p2

-- |Function that draws a rectangle between two points
drawRect :: (Num a, VertexComponent a) => Point a -> Point a -> IO ()
drawRect = drawRect' LineLoop

-- |Functions that fills a rectangle between two points
fillRect :: (Num a, VertexComponent a) => Point a -> Point a -> IO ()
fillRect = drawRect' Quads

drawRect' :: (Num a, VertexComponent a) =>
             PrimitiveMode -> Point a -> Point a -> IO ()
drawRect' prim p1 p2 = renderPrimitive prim
    $ mapM_ (\(x,y) -> vertex $ Vertex3 x y 0 )
    $ rectV p1 p2

-- |Function that draws a circle centered at the point with a radius
drawCircle :: (Floating a, Enum a, VertexComponent a) => Point a -> a -> IO ()
drawCircle = drawCircle' LineLoop

-- |Function that fills a circle centered around a point with a radius
fillCircle :: (Floating a, Enum a, VertexComponent a) => Point a -> a -> IO ()
fillCircle = drawCircle' Polygon

drawCircle' :: (Floating a, Enum a, Num a, VertexComponent a) =>
               PrimitiveMode -> Point a -> a -> IO ()
drawCircle' prim p1 r = renderPrimitive prim
    $ mapM_ (\(x,y) -> vertex $ Vertex3 x y 0 )
    $ circleV p1 r

-- |Function that draws a triangle with 3 points
drawTriangle :: (Floating a, Enum a, VertexComponent a)
             => Point a -> Point a -> Point a -> IO ()
drawTriangle = drawTriangle' Triangles

-- |Function that fills a circle centered around a point with a radius
fillTriangle :: (Floating a, Enum a, VertexComponent a)
             => Point a -> Point a -> Point a -> IO ()
fillTriangle = drawTriangle' Polygon

drawTriangle' :: (Num a, VertexComponent a) =>
                 PrimitiveMode -> Point a -> Point a -> Point a -> IO ()
drawTriangle' prim p1 p2 p3 = renderPrimitive prim
    $ mapM_ (\(x,y) -> vertex $ Vertex3 x y 0 ) [p1, p2, p3]



-- Vertex generating functions
lineV :: Point a -> Point a -> [Point a]
lineV p1 p2 = [p1, p2]

rectV :: Point a -> Point a -> [Point a]
rectV (x1,y1) (x2,y2) = [(x1,y1), (x1,y2), (x2,y2), (x2,y1)]

circleV :: (Floating a, Enum a) => Point a -> a -> [Point a]
circleV (x,y) r = [(x + r * cos t, y + r * sin t) | t <- [0,(pi/16)..2*pi] ]
