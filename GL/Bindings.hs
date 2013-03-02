module GL.Bindings 
    ( display, idle, reshape )
where

import Control.Applicative  ( (<$>) )
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT     ( swapBuffers, postRedisplay )

import GL.Aliases           ( glfloat, zerof, onef, setColor4 )

import qualified Program.State as P

-- |Callback to draw on screen
display :: P.State -> IO ()
display state = do
    clearColor $= Color4 zerof zerof zerof onef
    clear [ ColorBuffer, DepthBuffer ]

    draws <- get $ P.drawList state
    width <- glfloat <$> get (P.width state)
    height <- glfloat <$> get (P.height state)

    loadIdentity
    translate $ Vector3 (-onef) onef zerof
    scale (2/width) (-2/height) onef
    setColor4 0 0 0 1
    preservingMatrix $ do
        sequence_ draws
        return ()
    swapBuffers


-- |Callback for disle state between renders (display calls)
idle :: P.State -> IO ()
idle _ = postRedisplay Nothing


-- |Callabck in the event of window size change
reshape :: P.State -> Size -> IO ()
reshape state size@(Size w h) = do
    P.width state $= w
    P.height state $= h
    viewport $= (Position 0 0, size)
