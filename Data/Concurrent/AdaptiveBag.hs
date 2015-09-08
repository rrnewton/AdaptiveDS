{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Concurrent.AdaptiveBag
       (
         AdaptiveBag
       , newBag
       , newBagThreshold
       , add
       , remove
       )
       where

import Control.Concurrent
import Control.Monad
import Data.Atomics
import Data.Atomics.Vector
import Data.IORef
import qualified Data.Vector.Mutable as V
import Unsafe.Coerce

-- import Data.Concurrent.ScalableBag (ScalableBag)
-- import qualified Data.Concurrent.ScalableBag as SB

import Data.Concurrent.ScalableBagBoxed (ScalableBag)
import qualified Data.Concurrent.ScalableBagBoxed as SB

data Hybrid a = Pure { thresh :: Int, current :: ![a] }
              | Trans ![a] !(ScalableBag a)
              | LockFree !(ScalableBag a)

type AdaptiveBag a = IORef (Hybrid a)

newBag :: IO (AdaptiveBag a)
newBag = newBagThreshold 10

newBagThreshold :: Int -> IO (AdaptiveBag a)
newBagThreshold thresh = newIORef $ Pure thresh []

-- Push onto the bag.
add :: AdaptiveBag a -> a -> IO ()
add bag x = do
  tick <- readForCAS bag
  case peekTicket tick of
    Pure{thresh, current} ->
      let loop 0 _ = transition bag >> add bag x
          loop i t = do
            (success, t') <- casIORef bag tick $ Pure thresh (x:current)
            unless success $ loop (i-1) t'
      in loop thresh tick
    Trans xs bag -> SB.add bag x
    LockFree bag -> SB.add bag x

-- Attempt to pop from the bag, returning Nothing if it's empty.
remove :: AdaptiveBag a -> IO (Maybe a)
remove bag = do
  tick <- readForCAS bag
  case peekTicket tick of
    Pure _ [] -> return Nothing
    Pure thresh (x:xs) ->
      let loop 0 _ = transition bag >> remove bag
          loop i t = do
            (success, t') <- casIORef bag tick $ Pure thresh xs
            if success then return $ Just x else loop (i-1) t'
      in loop thresh tick
    Trans xs bag -> SB.remove bag
    LockFree bag -> SB.remove bag

transition :: AdaptiveBag a -> IO ()
transition bag = do
  tick <- readForCAS bag
  case peekTicket tick of
    Pure _ xs -> do
      caps <- getNumCapabilities
      scalable <- SB.newBag
      (success, _) <- casIORef bag tick (Trans xs scalable)
      when success $ do
        forkIO $ forM_ xs (SB.add scalable)
        casLoop_ bag (const $ LockFree scalable)
    _ -> return ()

casLoop_ :: IORef a -> (a -> a) -> IO ()
casLoop_ ref f = casLoop ref f'
  where f' x = (f x, ())

casLoop :: IORef a -> (a -> (a, b)) -> IO b
casLoop ref f = retryLoop =<< readForCAS ref
  where retryLoop tick = do
          let (new, ret) = f $! peekTicket tick
          (success, tick') <- casIORef ref tick new
          if success then return ret else retryLoop tick'
