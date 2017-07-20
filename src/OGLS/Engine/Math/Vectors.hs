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
{-# OPTIONS_GHC -fno-warn-orphans  #-}

-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Math.Vectors
-- description : 3D and 4D vectors arithmetic 
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : unstable 
-- Portability : POSIX
-------------------------------------------------------------------------------

module OGLS.Engine.Math.Vectors 
    ( (^$+), (^$-), (^$*)
    , dot, step 
    ) where 

#ifdef USE_GHC_GENERICS
import GHC.Generics               ( Generic (..) )
#endif
import Control.Applicative        ( liftA2       )
import Control.Monad              ( liftM2       )
import Control.Parallel           ( pseq         ) 
import Data.Complex               ( Complex (..) )
import Data.Orphans               (              )
import Control.Lens hiding ( (<.>) )

import OGLS.Engine.Math.Instances 


infixl 6 ^$+, ^$-
infixl 7 ^$*

default ()

class Vector a v where 
    -- not yet implemented
    
(^$+) :: forall (t :: * -> *) a. (Applicative t, Num a) => t a -> t a -> t a 
(^$-) :: forall (t :: * -> *) a. (Applicative t, Num a) => t a -> t a -> t a 
(^$*) :: forall (t :: * -> *) a. (Applicative t, Num a) => t a -> t a -> t a 


newtype E t = E { el :: forall x. Lens' (t x) x }

data Vec3 a = Vec3 { v3x :: {-# UNPACK #-} !a
                   , v3y :: {-# UNPACK #-} !a
                   , v3z :: {-# UNPACK #-} !a 
                   } deriving ( Eq, Ord, Show, Generic )
   
instance Functor Vec3 where 
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Applicative Vec3 where 
    pure x = Vec3 x x x 
    Vec3 f g h <*> Vec3 x y z = Vec3 (f x) (g y) (h z)


data Vec4 a = Vec4 { v4x :: {-# UNPACK #-} !a
                   , v4y :: {-# UNPACK #-} !a
                   , v4z :: {-# UNPACK #-} !a
                   , v4w :: {-# UNPACK #-} !a 
                   } deriving ( Eq, Ord, Show )


(^$+) = liftA2 (+)
{-# INLINE (^$+) #-}
(^$-) = liftA2 (-)
{-# INLINE (^$-) #-}
(^$*) = liftA2 (*)
{-# INLINE (^$*) #-}


-- | Left scalar product vector
(*^^) :: forall (f :: * -> *) a. (Functor f, Num a) => a -> f a -> f a
(*^^) a = fmap (a*)
{-# INLINE (*^^) #-}


-- | Right scalar product vector
(^^*) :: forall (f :: * -> *) a. (Functor f, Num a) => f a -> a -> f a
f ^^* a = fmap (*a) f
{-# INLINE (^^*) #-}


{- Needs Additive class and instances
basis :: (Traversable t, Num a) => [t a]
basis = basisFor (zero :: Additive v => v Int)

basisFor :: (Traversable t, Num a) => t b -> [t a]
basisFor = \t ->
   ifoldMapOf traversed ?? t $ \i _ ->
     return $ iover  traversed ?? t $ \j _ ->
         if i == j then 1 else 0
{-# INLINABLE basisFor #-}
-}

-- | Linear interpolation
lerp :: forall (f :: * -> *) a. (Functor f, Applicative f, Num a) => a -> f a -> f a -> f a
lerp alpha u v = alpha *^^ u ^$+ (1 - alpha) *^^ v
{-# INLINE lerp #-}


-- | Dot product 
dot :: forall (t :: * -> *) a. (Applicative t, Foldable t, Num a) => t a -> t a -> a 
dot v1 v2 = sum $ v1 ^$* v2


step :: forall (t :: * -> *) a. (Applicative t, Num a, Ord a) => t a -> t a -> t a
step = liftA2 (\a b -> if b < a then 0 else 1)

#ifdef HLINT
{-# ANN module "HLint: ignore Redundant lambda" #-}
#endif
