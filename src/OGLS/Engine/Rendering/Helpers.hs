{-# LANGUAGE Unsafe          #-}
{-# LANGUAGE BangPatterns 
           , TemplateHaskell #-}
-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Rendering.Helpers
-- Description : Helper functions 
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : experimental
-- Portability : POSIX
-------------------------------------------------------------------------------
module OGLS.Engine.Rendering.Helpers where

import Graphics.UI.GLUT
import Unsafe.Coerce    ( unsafeCoerce )

import Control.Parallel ( pseq, par    )


-- | TODO: TemplateHaskell


-- 3 element vectors
vector3f :: Float -> Float -> Float -> Vector3 GLfloat
vector3f x y z = Vector3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)

vector3d :: Double -> Double -> Double -> Vector3 GLdouble
vector3d x y z = Vector3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)



-- 3 element colors
color3f :: Float -> Float -> Float -> Color3 GLfloat
color3f x y z = Color3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)

color3d :: Double -> Double -> Double -> Color3 GLdouble
color3d x y z = Color3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)



vertex3f' :: (GLfloat, GLfloat, GLfloat) -> IO () 
vertex3f' (x,y,z) = vertex $ Vertex3 x y (z :: GLfloat)

color3f' :: (GLfloat, GLfloat, GLfloat) -> IO () 
color3f' (r,g,b) = color  $ Color3  r g (b :: GLfloat)



-- 3 normals colors
normal3f :: Float -> Float -> Float -> Normal3 GLfloat
normal3f x y z = Normal3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)

normal3d :: Double -> Double -> Double -> Normal3 GLdouble
normal3d x y z = Normal3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)



-- Scale
scalef :: Float -> Float -> Float -> IO ()
scalef x y z = scale (unsafeCoerce x :: GLfloat) 
                     (unsafeCoerce y :: GLfloat)
                     (unsafeCoerce z :: GLfloat)

scaled :: Double -> Double -> Double -> IO ()
scaled x y z = scale (unsafeCoerce x :: GLdouble) 
                     (unsafeCoerce y :: GLdouble)
                     (unsafeCoerce z :: GLdouble)



-- 2D texture coordinate
texCoord2f :: Float -> Float -> IO ()
texCoord2f x y = texCoord $ TexCoord2 (unsafeCoerce x :: GLfloat)
                                      (unsafeCoerce y :: GLfloat)

texCoord2d :: Double -> Double -> IO ()
texCoord2d x y = texCoord $ TexCoord2 (unsafeCoerce x :: GLdouble)
                                      (unsafeCoerce y :: GLdouble)


-- Convert given coordinate to vertex
toVertex :: Num a => (a, a) -> IO ()
toVertex (x,y) = vertex $ Vertex2 (unsafeCoerce x :: GLdouble) (unsafeCoerce y)


-- Make 2 element vertices
vertex2f :: Float -> Float -> Vertex2 GLfloat
vertex2f x y = Vertex2 (unsafeCoerce x) (unsafeCoerce y)

vertex2d :: Double -> Double -> Vertex2 GLdouble
vertex2d x y = Vertex2 (unsafeCoerce x) (unsafeCoerce y)


-- 3 element vertices
vertex3f :: Float -> Float -> Float -> Vertex3 GLfloat
vertex3f x y z = Vertex3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)

vertex3d :: Double -> Double -> Double -> Vertex3 GLdouble
vertex3d x y z = Vertex3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)



----------------------------------------------
-- Parallelism 
----------------------------------------------

-- | Strict map `
strictMap :: (a -> b) -> [a] -> [b]
strictMap f xs = force rwhnf xs `seq` map f xs  


-- | Some implementation from Control.Parallel.Strategies
-- I wanted to change a bit for gaining full control 
type Done = ()
type Strategy a = a -> Done 

-- | Normal form data
class NFData a where 
    rnf :: Strategy a 
    rnf = rwhnf 

-- | TODO: also TemplateHaskell
instance NFData (a -> b) where rnf !_ = ()
instance NFData Int      where rnf !_ = ()


-- | Unit reduce
r0 :: Strategy a 
r0 _ = ()

-- | Reduce to Weak Head Normal Form
rwhnf :: Strategy a 
rwhnf x = x `seq` () 

-- | Another helper 
using :: a -> Strategy a -> a 
using x s = s x `pseq` x 


-- | Force evaluation of the given list 
--force :: [a] -> ()
--force (x:xs) = x `pseq` force xs 
--force _      = ()
force :: Strategy a -> Strategy [a] 
force strategy (x:xs) = strategy x `par` (force strategy xs)

-- | Map the array in parallel
parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strategy f xs = map f xs `using` force strategy 


-- | Another abstraction over combination of mapping and folding 
mapReduce :: Strategy b  -- ^ Strategy of evaluating mapped result 
          -> (a -> b)    -- ^ Map function
          -> Strategy c  -- ^ Strategy of evaluating reduced result
          -> ([b] -> c)  -- ^ Reduce function
          -> [a]         -- ^ List to map over 
          -> c           -- ^ Output 
mapReduce mapStrategy mapFunction reduceStrategy reduceFunction input = 
    mapResult `pseq` reduceResult
  where mapResult    = parMap mapStrategy mapFunction input 
        reduceResult = reduceFunction mapResult `using` reduceStrategy
