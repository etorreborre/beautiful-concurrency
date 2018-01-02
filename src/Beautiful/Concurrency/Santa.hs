{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Beautiful.Concurrency.Santa where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Protolude              hiding (group, guard)
import           System.Random

data Gate = Gate Int (TVar Int)

data Group = Group Int (TVar (Int, Gate, Gate))

-- Create a new gate with a given size
newGate :: Int -> STM Gate
newGate gateCapacity = do
    -- the gate starts with no open slots
    tvar <- newTVar 0
    pure $ Gate gateCapacity tvar

-- Create a new group of a given size
-- with a size when the group is full (maximum size)
-- and a TVar holding the current group size
-- + 2 gates having the size of the group
newGroup :: Int -> IO Group
newGroup groupSize = atomically $ do
    inGate <- newGate groupSize
    outGate <- newGate groupSize
    tvar <- newTVar (0, inGate, outGate)
    pure $ Group groupSize tvar

-- Allow an elf or a reindeer to join a group
joinGroup :: Group -> IO (Gate, Gate)
joinGroup (Group _ tvar) = atomically $ do
  (remainingGroupSize, inGate, outGate) <- readTVar tvar
  check (remainingGroupSize > 0)
  writeTVar tvar (remainingGroupSize - 1, inGate, outGate)
  pure (inGate, outGate)

awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (Group groupSize tvar) =
  do (currentSize, inGate, outGate) <- readTVar tvar
     check (currentSize == groupSize)
     pure (inGate, outGate)

-- Allow an elf or a reindeer to pass a gate
passGate :: Gate -> IO ()
passGate (Gate _ tvar) = atomically $ do
  remainingGateSize <- readTVar tvar
  check (remainingGateSize > 0)
  writeTVar tvar (remainingGateSize - 1)

-- Allow Santo to open a gate
operateGate :: Gate -> IO ()
operateGate (Gate gateSize tvar) = do
  -- open the gate
  atomically $ writeTVar tvar gateSize
  -- wait for everyone to pass the gate
  atomically $ do
    remaining <- readTVar tvar
    check (remaining == 0)

doElfJob :: Group -> Int -> IO ()
doElfJob group elfId = doTheJob group (meetInStudy elfId)

doReindeerJob :: Group -> Int -> IO ()
doReindeerJob group reindeerId = doTheJob group (deliverToys reindeerId)

doTheJob :: Group -> IO () -> IO ()
doTheJob group task = do
    (inGate, outGate) <- joinGroup group
    passGate inGate
    task
    passGate outGate

meetInStudy :: Int -> IO ()
meetInStudy elfId = print $ "meeting " <> show @Int @Text elfId <> " in study"

deliverToys :: Int -> IO ()
deliverToys reindeerId = print $ "delivering toys with " <> show @Int @Text reindeerId

santa :: Group -> Group -> IO ()
santa elfs reindeers = do
    print "let's go"
    choose [(awaitGroup elfs, run "meet in study"), (awaitGroup reindeers, run "deliver toys")]
  where
    run :: Text -> (Gate, Gate) -> IO ()
    run task (inGate, outGate) = do
      print ("Ho, ho, ho, let's " <> task)
      operateGate inGate
      operateGate outGate

choose :: [(STM a, a -> IO ())] -> IO ()
choose choices =
  case foldr1May orElse actions of
    Nothing -> pure ()
    Just as -> join $ atomically as
  where
    actions :: [STM (IO ())]
    actions =
      do (guard, action) <- choices
         pure $ action <$> guard

elf :: Group -> Int -> IO ThreadId
elf group elfId = forkIO (forever $ do { doElfJob group elfId; randomDelay })

reindeer :: Group -> Int -> IO ThreadId
reindeer group reindeerId = forkIO (forever $ do { doReindeerJob group reindeerId; randomDelay })

randomDelay :: IO ()
randomDelay = do
  waitFor <- getStdRandom (randomR (1, 1000000))
  threadDelay waitFor

runAll :: IO ()
runAll = do
  elfGroup <- newGroup 3
  sequence_ [ elf elfGroup n | n <- [1..10] ]

  reindeerGroup <- newGroup 10
  sequence_ [ reindeer reindeerGroup n | n <- [1..9] ]

  forever (santa elfGroup reindeerGroup)
