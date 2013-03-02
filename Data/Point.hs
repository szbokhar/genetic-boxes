{-# LANGUAGE FlexibleInstances #-}

module Data.Point
    ( Point )
 where

-- |Type synonym for a 2-tuple
type Point a = (a,a)

-- |Type definition for a 2D euclidian point
newtype Pt a = Pt (a,a)

-- |Allows you to treat Points as 2D vectors using the
--  common numerical operators.
instance Num a => Num (Pt a) where
    Pt (x1,y1) + Pt (x2,y2) = Pt (x1+x2,y1+y2)
    Pt (x1,y1) - Pt (x2,y2) = Pt (x1-x2,y1-y2)
    Pt (x1,y1) * Pt (x2,y2) = Pt (x1*x2,y1*y2)
    abs (Pt (x,y)) = Pt (abs x,abs y)
    signum (Pt (x,y)) = Pt (signum x, signum y)
    fromInteger n = Pt (fromIntegral n,0)

-- |Converts a Pt to a 2-tuple
fromPt :: Pt a -> Point a
fromPt (Pt a) = a 
