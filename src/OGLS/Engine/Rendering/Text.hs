{-# LANGUAGE Trustworthy #-}
-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Rendering.Text
-- Description : Displays text on the current window
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : unstable 
-- Portability : POSIX
-------------------------------------------------------------------------------
module OGLS.Engine.Rendering.Text where

import Graphics.UI.GLUT

-- | Font to use
font :: BitmapFont
font = Helvetica12

-- | Print text to screen
screenPrint :: String -> IO ()
screenPrint = renderString font
