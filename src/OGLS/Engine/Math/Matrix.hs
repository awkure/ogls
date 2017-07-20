{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE CPP                   #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy           
           , DefaultSignatures     #-}
#define USE_GHC_GENERICS
#endif
{-# LANGUAGE RankNTypes            
           , TypeFamilies          
           , KindSignatures        
           , DeriveGeneric         
           , FlexibleInstances     
           , FlexibleContexts      
           , BangPatterns          
           , MultiParamTypeClasses #-}

-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Math.Matrix
-- description : 3x3 and 4x4 matrix arithmetic
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : unstable 
-- Portability : POSIX
-------------------------------------------------------------------------------

module OGLS.Engine.Math.Matrix where 
