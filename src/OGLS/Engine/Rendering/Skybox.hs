{-# LANGUAGE Trustworthy #-}
-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Rendering.Skybox
-- Description : Window callbacks handling 
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : experimental
-- Portability : POSIX
-------------------------------------------------------------------------------
module OGLS.Engine.Rendering.Skybox where 

import Graphics.UI.GLUT

import OGLS.Engine.Rendering.Helpers
import OGLS.Engine.Rendering.Textures



data Skybox = Skybox
    { front  :: Maybe TextureObject   -- ^ Front  side
    , right  :: Maybe TextureObject   -- ^ Right  side
    , back   :: Maybe TextureObject   -- ^ Back   side
    , left   :: Maybe TextureObject   -- ^ Left   side 
    , top    :: Maybe TextureObject   -- ^ Top    side
    , bottom :: Maybe TextureObject   -- ^ Bottom side
    } 



-- | Load textures needed for skybox initialization from `data/img/Skybox` directory
loadSkybox :: IO Skybox 
loadSkybox = do 
    ft <- loadTexture2D "data/img/Skybox/lmcity/lmcity_ft.tga"
    rt <- loadTexture2D "data/img/Skybox/lmcity/lmcity_rt.tga"
    bk <- loadTexture2D "data/img/Skybox/lmcity/lmcity_bk.tga"
    lf <- loadTexture2D "data/img/Skybox/lmcity/lmcity_lf.tga"
    up <- loadTexture2D "data/img/Skybox/lmcity/lmcity_up.tga"
    dn <- loadTexture2D "data/img/Skybox/lmcity/lmcity_dn.tga"
    return Skybox { front  = Just ft
                  , right  = Just rt
                  , back   = Just bk
                  , left   = Just lf
                  , top    = Just up
                  , bottom = Just dn 
                  }



-- | Make the given skybox from loaded texture objects 
drawSkybox :: Skybox -> IO ()
drawSkybox sky = do

    -- Turn off lighting and turn on textures
    lighting                 $= Disabled
    texture Texture2D        $= Enabled

    -- Draw front
    textureBinding Texture2D $= front sky
    preservingMatrix $ do
        rotate (-90.0) $ vector3f 1.0 0.0 0.0
        drawSide

    -- Draw right
    textureBinding Texture2D $= right sky
    preservingMatrix $ do
        rotate   90.0  $ vector3f 0.0 0.0 1.0
        rotate (-90.0) $ vector3f 1.0 0.0 0.0
        drawSide

    -- Draw back
    textureBinding Texture2D $= back sky
    preservingMatrix $ do
        rotate  180.0  $ vector3f 0.0 0.0 1.0
        rotate (-90.0) $ vector3f 1.0 0.0 0.0
        drawSide

    -- Draw left
    textureBinding Texture2D $= left sky
    preservingMatrix $ do
        rotate (-90.0) $ vector3f 0.0 0.0 1.0
        rotate (-90.0) $ vector3f 1.0 0.0 0.0
        drawSide

    -- Draw top
    textureBinding Texture2D $= top sky
    preservingMatrix $ do
        rotate (-180) $ vector3f 1.0 0.0 0.0
        drawSide

    -- Unbind the texture
    textureBinding Texture2D $= Nothing
    
  where drawSide :: IO ()
        drawSide = renderPrimitive Quads $ do
            normal $ normal3f 0.0 0.0 1.0
            texCoord2f 0.0 0.0
            vertex $ vertex3f (-1.0) (-1.0) (-0.99)
            texCoord2f 1.0 0.0
            vertex $ vertex3f   1.0  (-1.0) (-0.99)
            texCoord2f 1.0 1.0
            vertex $ vertex3f   1.0    1.0  (-0.99)
            texCoord2f 0.0 1.0
            vertex $ vertex3f (-1.0)   1.0  (-0.99)
