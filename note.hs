{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.IORef
import Data.Atomics
import Control.Concurrent

-- data Val t = V t | Tombstone
-- data EntryVal t = Val t | Copied (Val t)
-- Maybe we only need this one
data EntryRef t = Val (IORef t) | Copied (IORef t)
atomicModifyEntryRef' :: EntryRef a -> (a -> a) -> IO (a)

type Hybrid k v = IORef (HyState k v)
data HyState k v = A (S1 k v) 
                 | AB (S1 k v) (S2 k v)
                 | B (S2 k v)
                 | BA (S2 k v) (S1 k v)
            
initiateTransition :: Hybrid k v -> IO ()
initiateTransition r =
  do state <- readIORef r;
     case state of
       A s1 -> do emptyS2 <- newS2;
                  cas_state <- atomicModifyIORef' r (fn emptyS2);
                  case cas_state of
                    Just s1 -> do
                      forkIO (copyAB r s1 emptyS2)
                      return ()
                    Nothing -> return ()
               where
                 fn s2 x = case x of
                   A s1 -> (AB s1 s2, Just s1)
                   B s2 -> (B s2, Nothing)
                   AB s1 s2 -> (AB s1 s2, Nothing)
                   BA s2 s1 -> (BA s2 s1, Nothing)
       B s2 -> do emptyS1 <- newS1;
                  cas_state <- atomicModifyIORef' r (fn emptyS1);
                  case cas_state of
                    Just s2 -> do
                      forkIO (copyBA r s2 emptyS1)
                      return ()
                    Nothing -> return ()
               where
                 fn s1 x = case x of
                   A s1 -> (A s1, Nothing)
                   B s2 -> (BA s2 s1, Just s2)
                   AB s1 s2 -> (AB s1 s2, Nothing)
                   BA s2 s1 -> (BA s2 s1, Nothing)
       AB _ _ -> return ()
       BA _ _ -> return ()

get :: k -> Hybrid k v -> IO (Maybe v)
get k r =
    do state <- readIORef r;
       case state of
         A s1 -> return (getS1 k s1)
         B s2 -> getS2 k s2
         AB s1 s2 ->
             do x <- getS2 k s2;
                case x of
                  Nothing -> return (getS1 k s1)
                  Just a  -> return (Just a)
         BA s2 s1 ->
             case getS1 k s1 of
               Nothing -> (getS2 k s2)
               Just a -> return (Just a)

put :: k -> v -> Hybrid k v -> IO (Maybe v)
put k v r = do state <- readIORef r;
               case state of
                 A s1 -> undefined
                 B s2 -> undefined
                 AB s1 s2 -> undefined
                 BA s2 s1 -> undefined

copyAB :: Hybrid k v -> S1 k v -> S2 k v -> IO ()
copyAB = undefined

copyBA :: Hybrid k v -> S2 k v -> S1 k v -> IO ()
copyBA = undefined

data S1 k v = UndefinedS1
data S2 k v = UndefinedS2
newS1 :: IO (S1 k v)
newS1 = undefined
newS2 :: IO (S2 k v)
newS2 = undefined

-- need to modify put, replace atomicModifyIOref with tryModify
putS1 :: k -> v -> S1 k v -> IO (Maybe v) -- put function in S1
putS1 = undefined
putS2 :: k -> v -> S2 k v -> IO (Maybe v) -- put function in S2
putS2 = undefined

getS1 :: k -> S1 k v -> Maybe v -- put function in S1
getS1 = undefined
getS2 :: k -> S2 k v -> IO (Maybe v) -- put function in S2
getS2 = undefined
