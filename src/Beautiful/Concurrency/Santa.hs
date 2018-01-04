{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Beautiful.Concurrency.Santa where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Protolude              hiding (group, guard)
import           System.Random

data Gate = Gate Int (TVar Int)

data Group = Group {
  groupName :: Text
, groupSize :: Int
, state     :: TVar (Int, Gate, Gate)
}

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
newGroup :: Text ->  Int -> IO Group
newGroup name size = atomically $ do
    inGate <- newGate size
    outGate <- newGate size
    tvar <- newTVar (size, inGate, outGate)
    pure $ Group name size tvar

-- Allow an elf or a reindeer to join a group
joinGroup :: Group -> IO (Gate, Gate)
joinGroup (Group _ _ tvar) = atomically $ do
  (remainingGroupSize, inGate, outGate) <- readTVar tvar
  check (remainingGroupSize > 0)
  writeTVar tvar (remainingGroupSize - 1, inGate, outGate)
  pure (inGate, outGate)

-- Wait until a group is full
-- and reinitialize it
-- return the 2 original gates
awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (Group _ size tvar) =
  do (remainingSize, inGate, outGate) <- readTVar tvar
     check (remainingSize == 0)
     newInGate <- newGate size
     newOutGate <- newGate size
     writeTVar tvar (size, newInGate, newOutGate)
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
  printText "opening gate"
  -- open the gate
  atomically $ writeTVar tvar gateSize
  -- wait for everyone to pass the gate
  atomically $ do
    remaining <- readTVar tvar
    check (remaining == 0)

doElfJob :: Group -> Int -> IO ()
doElfJob group elfId = doTheJob elfId group (meetInStudy elfId)

doReindeerJob :: Group -> Int -> IO ()
doReindeerJob group reindeerId = doTheJob reindeerId group (deliverToys reindeerId)

doTheJob :: Int -> Group -> IO () -> IO ()
doTheJob id group task = do
    printAction id (" joined the group " <> groupName group)
    (inGate, outGate) <- joinGroup group
    passGate inGate
    printAction id (" passed in gate for group " <> groupName group)
    task
    printAction id (" finished task for group " <> groupName group)
    passGate outGate
    printAction id (" passed out gate for group " <> groupName group)

meetInStudy :: Int -> IO ()
meetInStudy elfId = print $ "meeting " <> show @Int @Text elfId <> " in study"

deliverToys :: Int -> IO ()
deliverToys reindeerId = print $ "delivering toys with " <> show @Int @Text reindeerId

santa :: Group -> Group -> IO ()
santa elfs reindeers = do
    printText "let's go"
    choose [(awaitGroup reindeers, run "deliver toys"), (awaitGroup elfs, run "meet in study")]
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
  elfGroup <- newGroup "elfs" 3
  sequence_ [ elf elfGroup n | n <- [1..10] ]

  reindeerGroup <- newGroup "reindeers" 5
  sequence_ [ reindeer reindeerGroup n | n <- [1..9] ]

  forever (santa elfGroup reindeerGroup)

printText :: Text -> IO ()
printText = print

printAction :: Int -> Text -> IO ()
printAction id t = printText (show id <> t)
