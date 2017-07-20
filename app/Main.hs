{-# LANGUAGE FlexibleContexts #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : Main application module 
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : unstable 
-- Portability : POSIX
-------------------------------------------------------------------------------
module Main where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLUT 
import Graphics.Rendering.OpenGL 
import Control.Exception ( catch, IOException )
import Data.IORef        ( newIORef, readIORef )
import OGLS.Core 

{-
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
 
newtype AppWindow a = AppWindow {
    runAW :: ReaderT AppBindings (StateT AppState IO) a 
} deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AppConfig
           , MonadState AppState )
-}

-- | Run application
run :: IO ()
run = do 
    -- Windiw initializing
    _ <- getArgsAndInitialize
    initialDisplayMode  $= [ RGBAMode, DoubleBuffered, WithDepthBuffer, Multisampling]
    initialWindowSize   $= Size 500 500
    _ <- createWindow "OGLS showcase alpha 0.0.1.1"
    
    -- Callbacks and state
    fpsSt <- initFPS
    state <- prepareState 
    reshapeCallback       $= Just reshape 
    keyboardMouseCallback $= Just (keyboardMouse state)
    displayCallback       $= primaryDisplay state 
    motionCallback        $= Just (motion state)  

    perWindowKeyRepeat  $= PerWindowKeyRepeatOn
    actionOnWindowClose $= Exit

    -- Antialiasing
    lineSmooth      $= Enabled 
    lineWidth       $= 1.3
    hint LineSmooth $= Nicest

    -- Texturing
    texture Texture2D $= Enabled 
    blend             $= Enabled 
    blendFunc         $= (SrcAlpha, OneMinusSrcAlpha)

    addTimerCallback timerFrequency (timer state)
  
    -- Shaders
    hasShaders <- readIORef $ useShaders state 
    if hasShaders
    then prepareShaders 
    else alternativeShading 

    printHelp
    --sky <- loadSkybox
    --drawSkybox sky

    depthFunc $= Just Less  
    drawHelp 
    nextBackground state
    nextMaterial   state
    
    -- Shows help because I don't want to import shitton of modules related to Text
    keyboardMouse state (Char '?') Down (Modifiers Up Up Up) (Position 0 0)
    --showFPS fpsSt (Just 0)
    
    mainLoop


{-
run :: IO ()
run = do 
    _ <- GLFW.init
    GLFW.defaultWindowHints

    title <- getTitle

    win <- GLFW.createWindow 800 600 title Nothing Nothing 
    GLFW.makeContextCurrent win 

    GL.shadeModel $= GL.Smooth

    state <- prepareState

    GLFW.setWindowRefreshCallback   win $ Just (primaryDisplay state)
    GLFW.setFramebufferSizeCallback win $ Just reshape
    GLFW.setKeyCallback             win $ Just (keyboardMouse state)
    GLFW.setWindowCloseCallback     win $ Just shutdown


shutdown :: GLFW.WindowCloseCallback
shutdown win = do 
    GLFW.destroyWindow win
    GLFW.terminate
    _ <- exitWith ExitSuccess
    return ()
-}

main :: IO ()
main = do run 
