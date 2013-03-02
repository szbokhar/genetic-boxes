module GL.Aliases 
    ( setColor4
    , glfloat, onef, zerof
    , int, glint, float
    , pointInt, pointGlint, pointFl
    , readInt )
where

import Graphics.Rendering.OpenGL   
                        ( GLint, GLfloat, Color(color), Color4(Color4) ) 

import Data.Point       ( Point )    

-- |Convienence function for setting the color without having to 
--  explicitly cast one of the arguments to GLfloat
setColor4 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
setColor4 r g b a = color $ Color4 r g b a

-- |Converts integral to GLfloat

-- |Number 1 casted as GLfloat
onef :: GLfloat
onef = 1.0
-- |Number 0 casted as GLfloat
zerof :: GLfloat
zerof = 0.0

-- |Casts to GLfloat
glfloat :: Integral a => a -> GLfloat
glfloat = fromIntegral
-- |Casts to GLint
glint :: Integral a => a -> GLint
glint = fromIntegral
-- |Casts to Int
int :: Integral a => a -> Int
int = fromIntegral
-- |Casts to Float
float :: Integral a => a -> Float
float = fromIntegral

-- |Casts Point a to Point GLfloat
pointFl :: Integral a => Point a -> Point GLfloat
pointFl (x,y) = (glfloat x, glfloat y)
-- |Casts Point a to Point GLint
pointGlint :: Integral a => Point a -> Point GLint
pointGlint (x,y) = (glint x, glint y)
-- |Casts Point a to Point Int
pointInt :: Integral a => Point a -> Point Int
pointInt (x,y) = (int x, int y)

-- |Reads an int
readInt :: String -> Maybe Int
readInt xs
    | null attempt  = Nothing
    | otherwise     = Just (fst $ head attempt)
  where attempt = (reads :: ReadS Int) xs
