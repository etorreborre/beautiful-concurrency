{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-|

Module      : Test.Tasty.Extensions
Description : Tasty / Hedgehog / HUnit integration

This module unifies property based testing with Hedgehog
and one-off tests.

-}
module Test.Tasty.Extensions where

import           GHC.Stack
import           Hedgehog                 as HH
import           Protolude                hiding ((.&.))
import           Test.QuickCheck.Checkers
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit         as H
import           Test.Tasty.QuickCheck    hiding (Property, testProperty)
import qualified Test.Tasty.QuickCheck    as QC
import           Test.Tasty.Runners
import           Text.StringConvert

-- | Create a Tasty test from a Hedgehog property
prop :: HasCallStack => TestName -> PropertyT IO () -> [TestTree]
prop name p = [withFrozenCallStack $ testProperty name (HH.property p)]

-- | Create a Tasty test from a Hedgehog property called only once
test :: HasCallStack => TestName -> PropertyT IO () -> [TestTree]
test name p = withFrozenCallStack $ minTestsOk 1 <$> prop name p

-- | Create a Tasty test from a simple description and a boolean value indicating the success
eg :: Text -> Bool -> [TestTree]
eg name b = [H.testCase (s name) (assertBool (s name) b)]

-- | Skip a Tasty test
skip :: [TestTree] -> [TestTree]
skip [tt] = let skipped name = [H.testCase ("SKIPPED - " <> name) (H.assertBool "" True)] in
  case tt of
    (SingleTest name _)   -> skipped name
    (TestGroup name _)    -> skipped name
    (PlusTestOptions _ t) -> skip [t]
    _                     -> skipped ""
skip _ = [H.testCase "SKIPPED" (H.assertBool "" True)]


-- * Parameters

minTestsOk :: Int -> (TestTree -> TestTree)
minTestsOk n = localOption (HedgehogTestLimit n)

instance Monoid QC.Property where
  mempty  = label "ok" True
  mappend = (.&.)
