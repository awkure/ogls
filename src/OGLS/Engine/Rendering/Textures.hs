{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase  #-}
-------------------------------------------------------------------------------
-- |
-- Module      : TOGLS.Engine.Rendering.Textures
-- Description : Texture rendering and initialization
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : unstable 
-- Portability : POSIX
-------------------------------------------------------------------------------
module OGLS.Engine.Rendering.Textures 
     ( checkImageSize
     , generateTextureMatrix
     , loadTexture2D
     , installTexture
     , lookat 
     , up 
     ) where 

import Graphics.UI.GLUT
import Graphics.GLUtil

import System.IO     ( hPutStrLn, stderr      )
import System.Exit   ( ExitCode(..), exitWith )
import Control.Monad ( liftM, when            )

import OGLS.Engine.Bindings
import OGLS.Engine.Rendering.Shaders.Light


-- | Just and alias of TextureSize2D
checkImageSize :: GLsizei -> GLsizei -> TextureSize2D
checkImageSize = TextureSize2D



-- | Variables that I intended to use alot in future
lookat :: Vertex3 GLdouble
lookat = Vertex3 0 0 0

up :: Vector3 GLdouble 
up = Vector3 0 0 1



-- | Wrapper around readTexture that crashes on error
loadTexture2D :: FilePath -> IO TextureObject 
loadTexture2D path = do 
    texture Texture2D $= Enabled 
    readTexture path >>= (\case 
        
        Left msg -> do 
            hPutStrLn stderr $ "Failed to load texture: " ++ path
            hPutStrLn stderr msg
            exitWith (ExitFailure 1)
        
        Right tex -> do 
            installTexture
            return tex )



-- | Setup a texture based on how 
installTexture :: IO ()
installTexture = do 
    (major,_) <- get . majorMinor $ glVersion
    if major < 3
        then do textureFilter Texture2D $= ((Linear',      Nothing), Linear')
        else do textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
                generateMipmap' Texture2D




-- | Set up projective texture matrix. I use the Modelbiew matrix stack and
--   OpenGL matrix commands to make the matrix.
generateTextureMatrix :: IO ()
generateTextureMatrix = do 
    m <- preservingMatrix $ do
         loadIdentity
         
         let translatef = translate :: Vector3 GLfloat -> IO ()
             scalef = scale :: GLfloat -> GLfloat -> GLfloat -> IO ()
         translatef (Vector3 0.5 0.5 0.0)
         scalef 0.5 0.5 1.0 
         perspective 60 1 1 1000
         lightPosition' <- getLightPosition Vertex3
         lookAt lightPosition' lookat up 
         get $ matrix $ Just ( Modelview 0 )

    [ sx, sy, sz, sw,
      tx, ty, tz, tw,
      rx, ry, rz, rw,
      qx, qy, qz, qw ] <- getMatrixComponents RowMajor (m :: GLmatrix GLdouble)

    textureGenMode S $= Just (ObjectLinear (Plane sx sy sz sw))
    textureGenMode T $= Just (ObjectLinear (Plane tx ty tz tw))
    textureGenMode R $= Just (ObjectLinear (Plane rx ry rz rw))
    textureGenMode Q $= Just (ObjectLinear (Plane qx qy qz qw))
