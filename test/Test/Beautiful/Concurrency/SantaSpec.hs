{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Beautiful.Concurrency.SantaSpec where

import           Hedgehog
import           Protolude
import           Test.Tasty.Extensions

test_ok = prop "ok" $
  True === True
