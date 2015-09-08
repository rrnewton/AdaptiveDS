{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This version is an experiment to try to figure out what is
-- destroying performance in ScalableBag

module Data.Concurrent.ScalableBagBoxed
       (
         ScalableBag
       , newBag
       , add
       , remove
       , osThreadID
       )
       where

import Control.Concurrent
import Data.Atomics
import Data.Bits
import Data.Atomics.Vector
import qualified Data.Atomics.Counter as C
import Data.TLS.PThread
import Data.Vector.Mutable as V
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Concurrent.PureBag as PB

--------------------------------------------------------------------------------

type ScalableBag a = IOVector (Maybe (PB.PureBag a))

-- {- NOINLINE osThreadID -}
osThreadID :: TLS Int
osThreadID = unsafePerformIO $ mkTLS $ C.incrCounter 1 threadCounter

-- {- NOINLINE threadCounter -}
threadCounter :: C.AtomicCounter
threadCounter = unsafePerformIO $ C.newCounter 0

-- {- INLINE newBag -}
newBag :: IO (ScalableBag a)
newBag = do
  caps <- getNumCapabilities
--  generateM caps (\_ -> fmap Just PB.newBag)
  V.replicate caps Nothing

-- {- INLINABLE add -}
add :: ScalableBag a -> a -> IO ()
add bag x = do
  tid <- getTLS osThreadID
  -- We try to keep this collision-free:
  let idx = tid `mod` (V.length bag)
  bg <- V.unsafeRead bag idx  
  case bg of
    Just b  -> PB.add b x
    Nothing -> do tick <- unsafeReadVectorElem bag idx
                  case peekTicket tick of
                    Just b -> PB.add b x
                    Nothing -> do
                      !b <- PB.newBag
                      (s,nt) <- casVectorElem bag idx tick (Just b)
                      -- If we fail it's only because someone else wrote it:
                      if s
                        then PB.add b x
                        else case peekTicket nt of
                               Nothing -> error "IMPOSSIBLE!"
                               Just b2 -> PB.add b2 x


-- {- INLINABLE remove -}
remove :: ScalableBag a -> IO (Maybe a)
-- remove bag = error "FINISHME - remove"a
remove bag = do
  tid <- getTLS osThreadID
  let -- Start with our own
      start = tid `mod` V.length bag      
      -- Sweep through all bags and try to "steal":
      retryLoop ix 
        | ix >= V.length bag = retryLoop 0 
        | otherwise =
          do peek <- V.unsafeRead bag ix
             case peek of
               -- Nothing here:
               Nothing ->
                 let ix' = ix + 1 in
                 if ix' == start || (ix' >= V.length bag && start == 0)
                 then return Nothing -- looped around once, nothing to pop
                 else retryLoop ix'  -- keep going
               Just b -> do
                 res <- PB.remove b
                 case res of
                   Nothing -> let ix' = ix + 1 in
                     if ix' == start || (ix' >= V.length bag && start == 0)
                     then return Nothing -- looped around once, nothing to pop
                     else retryLoop ix'  -- keep going
                   jx      -> return jx
  retryLoop start

-- FIXME: Vector.Mutable should really have generateM:
generateM :: Int -> (Int -> a) -> IO (IOVector a)
generateM num fn = do
  vec <- V.new num
  for_ 0 (num-1) $ \ix ->
    V.unsafeWrite vec ix (fn ix)
  return vec
{-# INLINE generateM #-}

-- Inclusive/Inclusive
for_ :: Monad m => Int -> Int -> (Int -> m a) -> m ()
-- for_ start end _ | start > end = error "start greater than end"
for_ start end fn = loop start
  where loop !i | i > end = return ()
                | otherwise = fn i >> loop (i+1)
{-# INLINE for_ #-}
