{-# LANGUAGE BangPatterns #-}
module Data.Concurrent.PureBag
       (
         PureBag
       , newBag
       , add
       , remove
       )
       where

import Data.Atomics
import Data.IORef

type PureBag a = IORef [a]

{-# INLINABLE newBag #-}
newBag :: IO (PureBag a)
newBag = newIORef []

{-# INLINABLE add #-}
add :: PureBag a -> a -> IO ()
add bag !x =   loop =<< readForCAS bag 
 where
 loop tik = do
   let !rst = peekTicket tik
   (success, t2) <- casIORef bag tik (x:rst)
   if success then return () else loop t2

{-# INLINABLE remove #-}
remove :: PureBag a -> IO (Maybe a)
remove bag = loop =<< readForCAS bag
 where
 loop tik = 
  case peekTicket tik of
   [] -> return Nothing
   (x:xs) -> do
     (success, t2) <- casIORef bag tik xs
     if success then return (Just x) else loop t2

-- remove bag = atomicModifyIORefCAS bag pop
--   where pop [] = ([], Nothing)
--         pop (x:xs) = (xs, Just x)
