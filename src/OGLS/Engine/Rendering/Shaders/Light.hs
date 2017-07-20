{-# LANGUAGE Trustworthy #-}
-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Rendering.Shaders.Light
-- Description : Light related utilities and initialization
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : experimental
-- Portability : POSIX
-------------------------------------------------------------------------------

module OGLS.Engine.Rendering.Shaders.Light 
    ( getLightPosition
    , prepareLight
    ) where 

import Graphics.UI.GLUT


-- | Aplly function to get light position
getLightPosition :: (GLdouble -> GLdouble -> GLdouble -> a) -> IO a 
getLightPosition f = do 
    Vertex4 x y z _ <- get $ position (Light 0)
    return $ f (realToFrac x) (realToFrac y) (realToFrac z)


-- | Initialize lightning
prepareLight :: IO ()
prepareLight = do 
    ambient (Light 0) $= Color4 (0.1*intesity)
                                (0.1*intesity)
                                (0.1*intesity)
                                1
    diffuse (Light 0) $= Color4 intesity
                                intesity
                                (0.8*intesity)
                                1
    position (Light 0) $= Vertex4 (realToFrac x) 
                                  (realToFrac y)
                                  (realToFrac z)
                                  1
    lightModelAmbient     $= Color4 0.02 0.02 0.02 1 
    lightModelLocalViewer $= Disabled 

    lighting           $= Enabled 
    light (Light 0)    $= Enabled
    
  where inclination = 20.58
        elevation   = 50.24
        intesity    = max 0.05 (realToFrac $ 0.5 + sin (elevation * pi/180))
        x           = 1000 * cos (elevation * pi/180) * sin (inclination * pi/180)
        y           = 1000 * sin (elevation * pi/180) * cos (inclination * pi/180)
        z           = 1000 * sin (elevation * pi/180) * sin (inclination * pi/180)
