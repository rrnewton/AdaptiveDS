{-# LANGUAGE BangPatterns #-}

-- The old way of doing things: atomicModifyIORef

module OldBag
       (
         PureBag
       , newBag
       , add
       , remove
       )
       where

import Data.Atomics
import Data.IORef
import EntryVal

type PureBag a = IORef [a] ::

{-# INLINABLE newBag #-}
newBag :: IO (PureBag a)
newBag = newIORef []

{-# INLINABLE add #-}
add :: PureBag a -> a -> IO ()
add bag !x = atomicModifyIORef' bag (\l -> (x:l,()))

{-# INLINABLE remove #-}
remove :: PureBag a -> IO (Maybe a)
remove bag = atomicModifyIORef' bag fn
 where
  fn [] = ([],Nothing)
  fn (h:t) = (t,Just h)
