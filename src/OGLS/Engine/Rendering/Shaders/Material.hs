{-# LANGUAGE Trustworthy #-}
-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Rendering.Shaders.Material
-- Description : Material related utilities and initialization
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : experimental
-- Portability : POSIX
-------------------------------------------------------------------------------

module OGLS.Engine.Rendering.Shaders.Material where 

import Graphics.UI.GLUT
import OGLS.Engine.Bindings


-- | Initialize material
prepareMaterial :: Material -> IO () 
prepareMaterial material = do
    materialAmbient   Front $= mAmbient   material 
    materialDiffuse   Front $= mDiffuse   material 
    materialSpecular  Front $= mSpecular  material 
    materialShininess Front $= mShininess material
    materialEmission  Front $= mEmission  material


-- | Cycle materials, used in `keyboardMouse` callback
nextMaterial :: State -> IO ()
nextMaterial state = do 
    mat <- get $ matCycle state 
    prepareMaterial $ head mat 
    matCycle state $~ tail

