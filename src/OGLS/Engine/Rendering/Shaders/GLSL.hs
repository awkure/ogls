{-# LANGUAGE Trustworthy #-}
-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Rendering.Shaders.GLSL
-- Description : Shaders related utilities and initialization 
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : unstable 
-- Portability : POSIX
-------------------------------------------------------------------------------

module OGLS.Engine.Rendering.Shaders.GLSL 
    ( checkGLSLsupport
    , compileShaders
    , installExampleShaders
    , toggleShading
    , alternativeShading
    , prepareShaders
    ) where 

import Graphics.UI.GLUT

import qualified Data.ByteString as B 
import           Control.Monad     ( unless             )
import           Control.Exception ( IOException, catch )

import OGLS.Engine.Bindings
import OGLS.Engine.Rendering.Shaders.Light
import OGLS.Engine.Rendering.Shaders.Material ()


-- | Checks GLSL support
checkGLSLsupport :: IO ()
checkGLSLsupport = do
    version <- get $ majorMinor glVersion
    unless (version >= (2,0)) $ do 
        extensions <- get glExtensions
        unless ("GL_ARB_shading_language_100" `elem` extensions) $
            ioError (userError "No GLSL support found.")


-- | Reads the given shader 
compileShaders :: ShaderType -> FilePath -> IO Shader 
compileShaders st fp = do 
    src    <- B.readFile   fp
    shader <- createShader st
    
    shaderSourceBS shader $= src 
    compileShader shader 
    reportErrors 
    
    ok  <- get $ compileStatus shader 
    log' <- get $ shaderInfoLog shader 
    
    mapM_ putStrLn ["Shader info log for '" ++ fp ++ "':", log', ""]
    
    unless ok $ do 
        deleteObjectNames [shader]
        ioError $ userError "compileShaders: shader compilation failed"
    
    return shader 


-- | Toggles shading via `keyboardMouse` callback
toggleShading :: State -> IO () 
toggleShading state = do 
    hasShaders <- get $ useShaders state 
    if hasShaders 
        then prepareShaders 
        else alternativeShading
    useShaders state $~! not


-- | Install loaded shaders 
installExampleShaders :: [Shader] -> IO ()
installExampleShaders shaders = do 
    brickProgram <- createProgram
    
    attachedShaders brickProgram $= shaders 
    linkProgram brickProgram
    reportErrors 
    
    ok <- get $ linkStatus brickProgram
    infoLog <- get $ programInfoLog brickProgram
    
    mapM_ putStrLn ["Program log:", infoLog, ""]
    
    unless ok $ do 
        deleteObjectNames [brickProgram]
        ioError $ userError "linking failed"

    currentProgram $= Just brickProgram

    let setUniform var val = do 
            location <- get $ uniformLocation brickProgram var
            reportErrors 
            uniform location $= val

    setUniform "MainColor"      $ Color3  1.0   0.3 ( 0.2 :: GLfloat)
    setUniform "SecondaryColor" $ Color3  0.85 0.86 (0.84 :: GLfloat)
    setUniform "Size"           $ Vertex2 0.30      (0.15 :: GLfloat)
    setUniform "Pct"            $ Vertex2 0.90      (0.85 :: GLfloat)
    setUniform "LightPosition"  $ Vertex3 0       0 (   1 :: GLfloat) 


-- | Alternative shading using built-in GLUT bindings
alternativeShading :: IO ()
alternativeShading = do 
    putStrLn "Using fixed function pipeline"
    prepareLight
    --prepareMaterial


-- | Initialize alternative shading
prepareShaders :: IO ()
prepareShaders = catch 
    (do checkGLSLsupport 
        vs <- compileShaders VertexShader   $ vertexShaderFilePath   shaderFilePath
        fs <- compileShaders FragmentShader $ fragmentShaderFilePath shaderFilePath
        installExampleShaders [vs, fs])
    (\exception -> do 
        print (exception :: IOException)
        alternativeShading) 

