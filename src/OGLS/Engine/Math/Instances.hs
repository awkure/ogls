{-# LANGUAGE CPP  #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif 
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

module OGLS.Engine.Math.Instances where 

import Data.Hashable              ( Hashable (..)         )
import Control.Monad.Fix          ( MonadFix (..)         )
import Control.Monad.Zip          ( MonadZip (..)         )
import Control.Applicative        ( liftA2                )
import Data.Functor.Bind          ( Apply (..), Bind (..) )
import Data.Semigroup.Foldable    ( Foldable1 (..)        )
import Data.Semigroup.Traversable ( Traversable1 (..)     )
import Data.Complex               ( Complex (..)          )
import Data.Semigroup             ( (<>)                  )
import Data.HashMap.Lazy as HashMap

instance (Hashable k, Eq k) => Apply (HashMap k) where
  (<.>) = HashMap.intersectionWith id

instance (Hashable k, Eq k) => Bind (HashMap k) where
  m >>- f = HashMap.fromList $ do
    (k, a) <- HashMap.toList m
    case HashMap.lookup k (f a) of
      Just b -> [(k,b)]
      Nothing -> []

instance Apply Complex where
  (a :+ b) <.> (c :+ d) = a c :+ b d

instance Bind Complex where
  (a :+ b) >>- f = a' :+ b' where
    a' :+ _  = f a
    _  :+ b' = f b
  {-# INLINE (>>-) #-}

instance MonadZip Complex where
  mzipWith = liftA2

instance MonadFix Complex where
  mfix f = (let a :+ _ = f a in a) :+ (let _ :+ a = f a in a)

instance Foldable1 Complex where
  foldMap1 f (a :+ b) = f a <> f b
  {-# INLINE foldMap1 #-}

instance Traversable1 Complex where
  traverse1 f (a :+ b) = (:+) <$> f a <.> f b
  {-# INLINE traverse1 #-}


