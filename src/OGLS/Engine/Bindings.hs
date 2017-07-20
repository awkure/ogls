{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE QuasiQuotes       
           , BangPatterns 
           , FlexibleInstances #-}

-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Bindings
-- Description : Global variables and bindings
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : experimental
-- Portability : POSIX
-------------------------------------------------------------------------------

module OGLS.Engine.Bindings  
    ( State          (..) 
    , ShaderFilePath (..)
    , Material       (..)
    , shaderFilePath
    , timerFrequency 
    , toVertex
    , showIO
    , getTime
    , prepareState
    , nextBackground
    , scaleIncrement
    , scaleFactor
    , resetState
    , runInertia
    , toggleRotation
    , help
    , printHelp
    , force 
    ) where 


import Prelude hiding ( sum )
import Graphics.UI.GLUT  
import Unsafe.Coerce         ( unsafeCoerce                    )

import NeatInterpolation     ( text                            )
import Data.IORef            ( newIORef, readIORef, IORef (..) ) 
import Data.Text             ( Text, unpack                    )
import Data.Foldable         ( Foldable, sum                   )
import Data.Time.Clock.POSIX ( getPOSIXTime                    )
import System.Exit           ( ExitCode (..)                   )
import Control.Monad         ( when                            )
import Control.Parallel      ( pseq                            )

import OGLS.Engine.Rendering.Primitives 
import OGLS.Engine.Rendering.Helpers
import OGLS.Engine.Rendering.Text
import OGLS.Engine.Math.Vectors 


data ShaderFilePath = ShaderFilePath 
        { vertexShaderFilePath   :: FilePath  -- ^ Path to .vert file
        , fragmentShaderFilePath :: FilePath  -- ^ Path to .frag file
        } deriving Show

shaderFilePath :: ShaderFilePath
shaderFilePath = ShaderFilePath 
        { vertexShaderFilePath   = "data/shaders/Example.vert"
        , fragmentShaderFilePath = "data/shaders/Example.frag" 
        }

inertiaThreshold = 1                     -- inertia threshold of the rotating object, used for calculating inertia
inertiaFactor    = 0.5                   -- inertia  factor of the rotating object, used for calculating inertia

scaleFactor :: Float                     -- scale factor of the rotating object, used for calculating inertia
scaleFactor = 0.01

scaleIncrement :: Float                  -- scale increment of the rotating object, used for calculating inertia
scaleIncrement = 0.5

initialDiff :: Vector3 GLfloat           -- initial diff of rotating , used for calculating inertia
initialDiff = Vector3 195 13 5

initialInertia :: Vector3 GLfloat        -- I'll do a proper documentation someday later 
initialInertia = Vector3 0.7 (-0.7) 0.7

timerFrequency :: Timeout 
timerFrequency = 20

shadowMapSize :: TextureSize2D
shadowMapSize = TextureSize2D 256 256

models :: [ IO () ]                      -- ^ list of models to render into the scene 
models = 
    [ cube 0.4                                   
    , renderObject Solid Octahedron             
    , renderObject Solid Tetrahedron           
    , renderObject Solid (Teapot 0.6)          
    , renderObject Solid (Sphere' 0.6 64 64)   
    , renderObject Solid (Torus 0.2 0.6 64 64) ] 


backgrounds :: [ Color4 GLclampf ]       -- ^ list of available backgrounds and (in the future) skyboxes used to render 
backgrounds = 
    [ Color4 0.0 0.0 0.0 1 
    , Color4 0.6 0.5 0.4 0 
    , Color4 0.7 0.7 0.7 1 
    , Color4 0.4 0.6 0.6 1 ]



data MaterialName = Bronze 
                  | Chrome 
                  | Gold 
                  | Pewter 
                  | Pearl 
                  | Turquoise 
                  deriving Show      

data Material = Material 
    { mName       :: MaterialName                      -- ^ Name of material being used 
    , mAmbient    :: {-# UNPACK #-} !(Color4 GLfloat)  -- ^ Ambient  map of the given material
    , mDiffuse    :: {-# UNPACK #-} !(Color4 GLfloat)  -- ^ Diffuse  map of the given material
    , mSpecular   :: {-# UNPACK #-} !(Color4 GLfloat)  -- ^ Specular map of the given material
    , mEmission   :: {-# UNPACK #-} !(Color4 GLfloat)  -- ^ Emission ratio of the given material
    , mShininess  :: {-# UNPACK #-} !GLfloat           -- ^ Shininess ration of the given material
    , mRefraction :: {-# UNPACK #-} !GLfloat           -- ^ Refraction ratio of the given material
    } deriving Show  


-- | Initialize materials
materials :: [ Material ] 
materials = [ Material { mName       = Bronze 
                       , mAmbient    = Color4 0.2125   0.1275   0.054    1
                       , mDiffuse    = Color4 0.714    0.4284   0.18144  1
                       , mSpecular   = Color4 0.393548 0.271906 0.166721 1
                       , mEmission   = Color4 0        0        0        1
                       , mShininess  = 25.6 
                       , mRefraction = 1.3 }
            , Material { mName       = Chrome
                       , mAmbient    = Color4 0.25 0.25 0.25 1
                       , mDiffuse    = Color4 0.4  0.4  0.4  1
                       , mSpecular   = Color4 0.4  0.4  0.4  1
                       , mEmission   = Color4 0.1  0.2  0.1  1
                       , mShininess  = 76.8 
                       , mRefraction = 1.51 } 
            , Material { mName       = Gold 
                       , mAmbient    = Color4 0.24725  0.1995   0.0745   1
                       , mDiffuse    = Color4 0.75164  0.60648  0.22648  1
                       , mSpecular   = Color4 0.628281 0.555802 0.366065 1
                       , mEmission   = Color4 0        0        0        1
                       , mShininess  = 51.2 
                       , mRefraction = 1.67 }
            , Material { mName       = Pewter
                       , mAmbient    = Color4 0.105882 0.058824 0.113725 1
                       , mDiffuse    = Color4 0.427451 0.470588 0.541176 1
                       , mSpecular   = Color4 0.333333 0.333333 0.521569 1
                       , mEmission   = Color4 0        0        0        1
                       , mShininess  = 9.84615 
                       , mRefraction = 1.2 }
            , Material { mName       = Pearl
                       , mAmbient    = Color4 0.25     0.20725  0.20725  0.822
                       , mDiffuse    = Color4 1        0.829    0.829    0.822
                       , mSpecular   = Color4 0.296648 0.296648 0.296648 0.822
                       , mEmission   = Color4 0.253112 0.1869   0.1879   0 
                       , mShininess  = 11.264 
                       , mRefraction = 1.35 }
            , Material { mName       = Turquoise
                       , mAmbient    = Color4 0.1      0.18725 0.1745   0.95
                       , mDiffuse    = Color4 0.396    0.74151 0.69102  0.95
                       , mSpecular   = Color4 0.297254 0.30829 0.306678 0.95
                       , mEmission   = Color4 0        0       0        1
                       , mShininess  = 12.8 
                       , mRefraction = 1.77} ]


data State = State 
    { angle        :: IORef GLdouble                   -- ^ Angle of inclination of particular model
    , scaleto      :: IORef GLfloat                    -- ^ I use scaling model instead of moving camera aroind it
    , delta        :: IORef GLfloat                    -- ^ Needed for calculating inertia
    , inertia      :: IORef (Vector3 GLfloat)          -- ^ Initial inertia of the given model
    , inertiaOld   :: IORef (Vector3 GLfloat)          -- ^ Buffer for proper calculation
    , diff         :: IORef (Vector3 GLfloat)          -- ^ Difference of inclination while rotating model 
    , shouldRotate :: IORef Bool                       -- ^ Rotation toggle (on/off) 
    , useShaders   :: IORef Bool                       -- ^ Shading  toggle (on/off)
    , point        :: IORef Int                        -- ^ Don't remember where I used it 
    , modelsCycle  :: IORef [IO ()]                    -- ^ Where models stored
    , bgCycle      :: IORef [Color4 GLclampf]          -- ^ Where backgrounds/skyboxes stored
    , matCycle     :: IORef [Material]                 -- ^ Where materials stored
    , lastPosition :: IORef Position                   -- ^ I'm so fucking tired 
    , lastIncr     :: IORef (Vector3 GLfloat)          -- ^ Last increment while rotating model
    , modifiers    :: IORef Modifiers                  -- ^ Needed for proper keyboardMouse callback work
    , pos          :: IORef (GLfloat, GLfloat)         -- ^ I wanted to move model around the scene but it's unimplemented rn
    }       


-- | Initialize state
prepareState :: IO State 
prepareState = do 
    an <- newIORef 0.0
    dl <- newIORef 0.1
    sc <- newIORef 1
    pt <- newIORef 20
    ps <- newIORef (0, 0)
    io <- newIORef (pure 0)
    li <- newIORef (pure 0)
    sr <- newIORef True
    sh <- newIORef False 
    di <- newIORef initialDiff
    ia <- newIORef initialInertia
    mc <- newIORef (cycle models)
    cc <- newIORef (cycle backgrounds)
    mt <- newIORef (cycle materials)
    mo <- newIORef (Modifiers Up Up Up)
    lp <- newIORef (Position (-1) (-1))
    return $ State 
        { diff         = di
        , modelsCycle  = mc
        , bgCycle      = cc
        , matCycle     = mt
        , scaleto      = sc 
        , delta        = dl
        , inertia      = ia
        , inertiaOld   = io
        , angle        = an
        , shouldRotate = sr
        , useShaders   = sh
        , modifiers    = mo
        , lastPosition = lp
        , lastIncr     = li
        , pos          = ps
        , point        = pt 
        }
    

-- | Outputs help to stdout
help :: Text 
help = [text|

    Keyboard commands:
    
    b − Toggle background color
    t - Toggle models to render
    m − Toggle materials to render 
    
    <space>, <click>          - stop rotation
    <+>, <-> or <ctrl + drag> - zoom model
    <arrow keys> <drag>       - rotate model
    
    ? - Help
    q, <Esc> - Exit

|]


----------------------------------------------
-- Some helper utilities 
----------------------------------------------

-- | Pointless trying to draw text on the window
printHelp :: IO ()
printHelp = do 
    windowPos   $ vertex2f 5.0 5.0
    screenPrint $ unpack help
{-# INLINE printHelp #-}



-- | Toggles background color/skybox
nextBackground :: State -> IO ()
nextBackground state = do 
    cc <- get $ bgCycle state 
    clearColor $= head cc 
    bgCycle state $~ tail
{-# NOINLINE nextBackground #-}


-- | Calculates inertia using given state
runInertia :: State -> IO ()
runInertia state = do 
    lastPosition state $= Position (-1) (-1)
    
    li <- get $ lastIncr state
    ia <- get $ inertia  state 
    
    let t = pure inertiaThreshold
        f = pure inertiaFactor
        l = (pure 1 ^$- (step (fmap negate t) li)) ^$* ((li ^$+ t) ^$* f ^$- ia)
        r = (step t li) ^$* ((li ^$- t) ^$* f ^$- ia)

    inertia  state $= l ^$+ ia ^$+ r
    lastIncr state $= pure 0
{-# NOINLINE [0] runInertia #-}



-- | Toggles rotation for the given state
toggleRotation :: State -> IO ()
toggleRotation state = do 
    rotation <- get $ shouldRotate state
    shouldRotate state $~ not 
    if rotation 
        then do
           ia <- get $ inertia state
           inertiaOld state $= ia
        else do 
           io <- get $ inertiaOld state 
           inertia state $= io
           when (dot io io == 0) $ 
               inertia state $= initialInertia



-- | Resets given state
resetState :: State -> IO () 
resetState state = do 
    diff     state $= initialDiff
    lastIncr state $= pure 0 
    inertia  state $= initialInertia
    scaleto  state $= 1
{-# INLINEABLE [0] resetState #-}


-- | Don't remember where I used it
getTime :: IO Double
getTime = (fromRational . toRational) <$> getPOSIXTime
{-# INLINEABLE [0] getTime #-}


-- | Shows contents of the given IO monad
showIO :: Show a => IO (a) -> IO ()
showIO = (>>= putStr . show)
{-# INLINE [~1] showIO #-}
