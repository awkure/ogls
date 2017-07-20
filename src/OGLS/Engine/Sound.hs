-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Sound
-- Description : Provides sound 
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : unstable 
-- Portability : POSIX
-------------------------------------------------------------------------------
module OGLS.Engine.Sound 
--{-# WARNING "Unimplemented" #-}
   ( SoundFlag (..) ) where 

import Foreign
import Data.Bits

data SoundFlag = Application
               | Alias
               | AliasId
               | Async
               | Filename
               | Loop
               | Memory
               | NoDefault
               | NoStop
               | NoWait
               | Purge
               | Resource
               | Sync
