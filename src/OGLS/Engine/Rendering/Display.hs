{-# LANGUAGE Trustworthy #-}
-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Rendering.Display
-- Description : Display related stuff like buffers handling, etc
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : unstable 
-- Portability : POSIX
-------------------------------------------------------------------------------
module OGLS.Engine.Rendering.Display 
    ( radius
    , primaryDisplay 
    , prepareFloor 
    ) where 

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GLU.Matrix

import Control.Monad ( forM_, when )
import Data.IORef    ( newIORef    )

import OGLS.Engine.Bindings
import OGLS.Engine.Rendering.Primitives 
import OGLS.Engine.Rendering.Textures

--import qualified Graphics.UI.GLFW as GLFW
--import Graphics.Rendering.OpenGL


-- | Useless rn
radius :: GLdouble
radius = 30



-- | Perhaps I had to move it to Callbacks module 
primaryDisplay :: State -> DisplayCallback
primaryDisplay state = do 
    loadIdentity
    translate ( Vector3 0 0 (-5 :: GLfloat) )  -- position of the camera also needs to check `scaleto`
    
    Vector3 xDiff yDiff zDiff <- get (diff state)
    rotate yDiff ( Vector3 1 0 0 )
    rotate xDiff ( Vector3 0 1 0 )
    rotate zDiff ( Vector3 0 0 1 )
    
    prepareFloor True

    sc <- get (scaleto state)
    scale sc sc sc
    
    clear [ ColorBuffer, DepthBuffer ]
    (drawModel:_) <- get (modelsCycle state)
    drawModel

    generateTextureMatrix

    --camera <- get camIO
    preservingMatrix $ do 
        angle' <- get (angle state)
        lookAt (Vertex3 (radius * cos angle') (radius * sin angle') 30) lookat up
        --lookAt (position camera) (lookAt camera) (upVector camera)

    flush
    swapBuffers


-- | Make 2D plane which represents floor of the scene (unused)
prepareFloor :: Bool -> IO ()
prepareFloor shadowRender = do 
    textureOn <- get $ texture Texture2D 

    when shadowRender $ 
        texture Texture2D $= Disabled   -- texture is disabled like you are 

    let normal3f = normal :: Normal3 GLfloat -> IO ()
        color3f  = color  :: Color3  GLfloat -> IO ()
        rect3f   = rect   :: Vertex2 GLfloat -> Vertex2 GLfloat -> IO ()

    normal3f ( Normal3 0 0 1 ) 
    color3f  ( Color3  1 1 1 ) 
    rect3f   ( Vertex2 (-20) (-20) ) ( Vertex2 20 20 ) 

    when (shadowRender && textureOn == Enabled) $ 
        texture Texture2D $= Enabled 
