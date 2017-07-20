{-# LANGUAGE Trustworthy      #-}
{-# LANGUAGE TupleSections 
           , NegativeLiterals #-}

-------------------------------------------------------------------------------
-- |
-- Module      : OGLS.Engine.Rendering.Primitives
-- Description : Simple models representation (which needs to be fixed)
-- Copyright   : (c) Adam, 2017
-- License     : MIT
-- Maintainer  : awkure@protonmail.ch 
-- Stability   : unstable 
-- Portability : POSIX
-------------------------------------------------------------------------------

module OGLS.Engine.Rendering.Primitives where 

import Graphics.UI.GLUT

import System.Random    ( getStdRandom
                        , random
                        , randomR
                        , StdGen (..)
                        , newStdGen       )
import System.IO.Unsafe ( unsafePerformIO )
import Data.Monoid      ( (<>)            )
import Data.List        ( unfoldr         )


-- | Generate points from the given count
points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
   where n' = fromIntegral n


-- | Random monadic point position 
randompos :: Int -> [(GLfloat,GLfloat,GLfloat)]
randompos n = let f  = (unsafePerformIO . getStdRandom . randomR)
                  n' = fromIntegral n
              in [(f (0,k), f (0,k), f (0,k)) | k <- [1..n']]


-- | Another variant of the previous function
randompos' :: Int -> [(GLfloat,GLfloat,GLfloat)]
randompos' n = atimes n $ [(f,f,f)]
    where f = (unsafePerformIO . getStdRandom . randomR) (0.0,3)


-- | Generate random list of Ints 
randomList :: Int -> IO [Int]
randomList n = do
    -- needs refactoring
    seed <- newStdGen
    let xs = genlist n seed 
    return xs 
  where genlist :: Int -> StdGen -> [Int]
        genlist n = take n . unfoldr ( Just . random ) -- take function casts type of n as Int 


-- | There should be the same function inside Monoid package somewhere 
atimes :: Monoid m => Int -> m -> m  
atimes n arr | n <= 0    = mempty
             | otherwise = arr <> atimes (n - 1) arr
{-# INLINEABLE atimes #-}


-- | Face
face :: Normal3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat 
     -> Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
face p q r s t = do
    let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
    normal p
    texCoord2f (TexCoord2 1 1)
    vertex q 
    texCoord2f (TexCoord2 0 1)
    vertex r
    texCoord2f (TexCoord2 0 0)
    vertex s
    texCoord2f (TexCoord2 1 0)
    vertex t 



-- | Cube
cube :: GLfloat -> IO ()
cube size = do 
    let a = Vertex3   size    size    size
        b = Vertex3   size    size  (-size)
        c = Vertex3   size  (-size) (-size)
        d = Vertex3   size  (-size)   size
        e = Vertex3 (-size)   size    size
        f = Vertex3 (-size)   size  (-size)
        g = Vertex3 (-size) (-size) (-size)
        h = Vertex3 (-size) (-size)   size

        i = Normal3   1    0    0 
        k = Normal3  -1    0    0
        l = Normal3   0    0   -1 
        m = Normal3   0    0    1
        n = Normal3   0    1    0
        o = Normal3   0   -1    0

    renderPrimitive Quads $ do 
        face i d c b a
        face k g h e f
        face l c g f b
        face m h d a e
        face n e a b f
        face o g c d h
