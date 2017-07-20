{-# LANGUAGE Unsafe #-}
-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Debug
-- Description : Draws debug information  
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : unstable 
-- Portability : POSIX
--
-- Deprecated, use OGLS.Engine.Rendering.Text insead
-------------------------------------------------------------------------------

module OGLS.Engine.Debug 
--{-# WARNING "Use OGLS.Engine.Rendering.Text instead" #-}
    ( initFPS
    , showFPS
    , showtext 
    , drawHelp
    ) where 

import Graphics.UI.GLUT

import Data.IORef    ( IORef (..), readIORef, writeIORef, newIORef )
import Control.Monad ( when, unless, forM_, liftM                  )
import Unsafe.Coerce ( unsafeCoerce                                )

import OGLS.Engine.Bindings


data FPS = FPS 
    { fpsTime  :: IORef Double     -- ^ 
    , fpsShown :: IORef Bool       -- ^ 
    , fpsLast  :: IORef [GLdouble] -- ^
    }


-- | Initialize FPS 
initFPS :: IO FPS
initFPS = do 
    t <- newIORef =<< getTime
    s <- newIORef True
    l <- newIORef []
    return FPS 
        { fpsTime  = t
        , fpsShown = s
        , fpsLast  = l
        }



-- | Draws some debug information on the window
showFPS :: FPS -> Maybe String -> IO ()
showFPS (FPS t s fps') add = do 
    toShow <- readIORef s 
    when toShow $ do 
        -- Calculate difference between runs and update list of differences 
        told <- readIORef t 
        tcur <- getTime 
        runs <- readIORef fps' 
        writeIORef t tcur
        writeIORef fps' $ unsafeCoerce (1 / (tcur - told)) : (take 59 runs)

        preservingMatrix $ do 
            -- Move to upper left corner 
            translate $ Vector3 0.02 0.89 (0.0 :: GLdouble) 
            scale 0.5 0.1 (0.1 :: GLdouble)

            -- Show values
            values <- readIORef fps'
            unless (null values) $ do 
                let steps  = 59.0
                    dsteps = 1.0 / steps

                color $ Color3 1 0 (0 :: GLdouble)
                renderPrimitive LineStrip $ 
                    forM_ (zip [1.0,(1.0-dsteps)..0] values) $ \(x,y) -> do 
                        let b = (x, min (y/steps) 0.99)
                        toVertex b

                -- Display textual information
                color $ Color3 1 1 (1 :: GLdouble)
                let str = take 5 (show (head values)) ++ maybe [] (" " ++) add
                showtext (0.004, 0.75) [str]


    

-- | Handle different window sizes correctly by calculating the space between
--   subsequent lines; probably should be in `OGLS.Engine.Rendering.Display` module  
showtext :: (GLfloat, GLfloat) -> [String] -> IO ()
showtext (x,y) xs' = do 
    Size _ wh <- get windowSize
    fs        <- (*1.0) `liftM` fontHeight Fixed9By15
    render (fs / fromIntegral wh) y xs'
  where render _ _ []      = return ()
        render h y' (xx:xs) = do
            rasterPos $ Vertex3 x y' 0.0
            renderString Fixed8By13 xx
            render h (y'-h) xs



-- | Won't work until I configure FGTL properly
drawHelp :: IO ()
drawHelp = do 
    color $ Color3 1 0.5 (1 :: GLdouble)
    showtext (0.9, 0.95) [ "test", "ses" ]
