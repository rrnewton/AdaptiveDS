{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.IORef

data Val t = V t | Tombstone
data EntryVal t = Val t | Copied (Val t)

type Hybrid k v = IORef (HyState k v)
data HyState k v = A (S1 k v) 
                 | AB (S1 k v) (S2 k v)
                 | B (S2 k v)
                 | BA (S2 k v) (S1 k v)
            
initiateTransition :: Hybrid k v -> IO ()
initiateTransition r = undefined

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

put :: k -> Hybrid k v -> IO (Maybe v)
put = undefined

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
-- need to add copy function in S1 and S2
copyS1 :: S1 k v -> [(k,v)] -- copy function in S1
copyS1 = undefined
copyS2 :: S2 k v -> [(k,v)] -- copy function in S2
copyS2 = undefined
-- need to modify put, replace atomicModifyioref with tryModify
putS1 :: k -> v -> S1 k v -> IO (Maybe v) -- put function in S1
putS1 = undefined
putS2 :: k -> v -> S2 k v -> IO (Maybe v) -- put function in S2
putS2 = undefined

getS1 :: k -> S1 k v -> Maybe v -- put function in S1
getS1 = undefined
getS2 :: k -> S2 k v -> IO (Maybe v) -- put function in S2
getS2 = undefined
