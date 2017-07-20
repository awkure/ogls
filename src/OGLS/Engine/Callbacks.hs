{-# LANGUAGE Trustworthy #-}
-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Callbacks
-- Description : Window callbacks handling 
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : experimental
-- Portability : POSIX
-------------------------------------------------------------------------------

module OGLS.Engine.Callbacks 
    ( reshape
    , motion
    , keyboardMouse
    , timer 
    ) where 

import Graphics.UI.GLUT 

import Control.Monad ( when )
import System.Exit   ( exitWith, ExitCode ( ExitSuccess ) )
import Data.Text     ( unpack )

import OGLS.Engine.Bindings
import OGLS.Engine.Math.Vectors 
import OGLS.Engine.Rendering.Shaders.GLSL
import OGLS.Engine.Rendering.Shaders.Material



-- | Handles window reshaping 
reshape :: ReshapeCallback 
reshape size@(Size w h) = do 
    let vp = 0.8
        aspect = fromIntegral w / fromIntegral h 
    
    viewport   $= (Position 0 0, size)
    matrixMode $= Projection
    
    loadIdentity
    frustum (-vp) vp (-vp / aspect) (vp / aspect) 3 10
    matrixMode $= Modelview 0
    
    loadIdentity
    
    translate $ Vector3 0 0 (-5 :: GLfloat)


-- | Handles mouse events/motion and how models should behave
motion :: State -> MotionCallback
motion state pos'@(Position x y) = do 
    postRedisplay Nothing
    Position xt yt <- get $ lastPosition state
    lastPosition state $= pos'
    
    when (xt /= -1 || yt /= -1) $ do 
        let li@(Vector3 xl yl _) = Vector3 (fromIntegral (x - xt)) (fromIntegral (y - yt)) 0
        lastIncr state $= li

        when (xt /= -1) $ do 
            mods <- get $ modifiers state 
            if ctrl mods == Down
               then do diff state $~ (^$+ Vector3 0 0 xl)
                       scaleto state $~ (+ (yl * scaleFactor))
               else diff state $~ (^$+ li)

    angle state $~! (+ (pi / 10000))
    


-- | Handles keyboard and mouse events 
keyboardMouse :: State -> KeyboardMouseCallback
keyboardMouse state key keystate modifications _ = do 
    modifiers state $= modifications
    postRedisplay Nothing
    
    case (key, keystate) of 
        (Char ' ', Down) -> toggleRotation state 
        (Char '+', Down) -> scaleto state $~! (+ scaleIncrement) 
        (Char '-', Down) -> scaleto state $~! (+ (negate scaleIncrement))
        (Char 'q', Down) -> exitWith ExitSuccess
        (Char 'b', Down) -> nextBackground state
        (Char 'm', Down) -> nextMaterial   state 
        (Char 't', Down) -> modelsCycle state $~! tail
        (Char 's', Down) -> toggleShading state 
        (Char  _ , Down) -> putStrLn $ unpack help
        
        -- Wanted to move model on Key buttons instead of rotating it
        --(SpecialKey KeyLeft  , Down) -> pos state $~! \(x,y) -> (x-0.1,y)
        --(SpecialKey KeyRight , Down) -> pos state $~! \(x,y) -> (x+0.1,y)
        --(SpecialKey KeyUp    , Down) -> pos state $~! \(x,y) -> (x,y+0.1)
        --(SpecialKey KeyDown  , Down) -> pos state $~! \(x,y) -> (x,y-0.1)
        (SpecialKey KeyHome  , Down) -> resetState state
        (SpecialKey KeyLeft  , Down) -> diff state $~ (^$- Vector3 3 0 0)
        (SpecialKey KeyRight , Down) -> diff state $~ (^$+ Vector3 3 0 0)
        (SpecialKey KeyUp    , Down) -> diff state $~ (^$- Vector3 0 3 0)
        (SpecialKey KeyDown  , Down) -> diff state $~ (^$+ Vector3 0 3 0)
        
        (MouseButton LeftButton, Down) -> do
            inertia  state $=! pure 0
            lastIncr state $=! pure 0
        (MouseButton LeftButton, Up) -> runInertia state 
        
        _ -> return ()

-- | Registers timer callback
timer :: State -> TimerCallback
timer state = do 
    rot <- get $ shouldRotate state 
    when rot $ do 
        ia <- get $ inertia state 
        diff state $~! (^$+ ia)
        postRedisplay Nothing
    addTimerCallback timerFrequency (timer state)
