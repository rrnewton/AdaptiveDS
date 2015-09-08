{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module Data.Concurrent.ScalableBagChaseLev
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
import Data.Concurrent.Deque.ChaseLev
import Data.TLS.PThread
import Data.Vector (Vector(..))
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

dbgPrint :: String -> IO ()
#if 0
dbgPrint s = putStrLn $ " [dbg] "++s
#else
dbgPrint _ = return ()
{-# INLINE dbgPrint #-}
#endif

--------------------------------------------------------------------------------

type ScalableBag a = Vector (ChaseLevDeque a)

{-# NOINLINE osThreadID #-}
osThreadID :: TLS Int
osThreadID = unsafePerformIO $ mkTLS $ C.incrCounter 1 threadCounter

{-# NOINLINE threadCounter #-}
threadCounter :: C.AtomicCounter
threadCounter = unsafePerformIO $ C.newCounter 0

-- | This padding factor prevents false sharing.  Cache lines on intel
-- are 64 bytes / 8 words.
padFactor :: Int
padFactor = 8

-- | Divide by the padding factor
padDiv :: Int -> Int
padDiv x = shiftR x 3

newBag :: IO (ScalableBag a)
newBag = do
  caps <- getNumCapabilities
  V.replicateM (padFactor * caps) newQ

add :: ScalableBag a -> a -> IO ()
add bag x = do
  tid <- getTLS osThreadID
  let !len = padDiv (V.length bag)
  dbgPrint$ "tid "++show tid++"Length of bag: "++show len
  -- We try to keep this collision-free:
  let !idx = tid `mod` len
      !idx' = padFactor * idx
  dbgPrint$ "tid "++show tid++" going into CAS loop on index "++show idx'
  pushL (bag V.! idx') x

remove :: ScalableBag a -> IO (Maybe a)
remove bag = do
  tid <- getTLS osThreadID
  let !idx = (tid `mod` V.length bag) * padFactor
      loop !ix start | ix >= V.length bag = loop 0 start
      loop !ix start = do
        elt <- tryPopR (bag V.! ix)
        case elt of
          Nothing -> let ix' = ix + padFactor in
            if ix' == start || (ix' >= V.length bag && start == 0)
            then return Nothing -- looped around once, nothing to pop
            else loop ix' start -- keep going
          jx -> return jx
  loop idx idx
