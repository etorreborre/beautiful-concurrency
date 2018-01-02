{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

Module      : Test.Tasty.Checks
Description : Tasty / Hedgehog support for testing IO and Either values

-}
module Test.Tasty.Checks
  ( isRight
  , isLeft
  , isRightIO
  , isLeftIO
  ) where

import           Control.Exception
import           Control.Monad
import           Data.Either.Combinators hiding (isLeft, isRight)
import           Hedgehog
import           Prelude                 (String)
import           Protolude               hiding (isLeft, isRight)
import           Text.StringConvert

attemptEither :: Show a => IO (Either a b) -> IO (Either Text b)
attemptEither io = fmap join (attempt (fmap (mapLeft (\a -> s (show a :: String))) io))

attempt :: IO a -> IO (Either Text a)
attempt io = handle (\(e :: SomeException) -> (pure . Left . s) (show e :: String)) (Right <$> io)

isRightIO :: Show a => IO (Either a b) -> PropertyT IO ()
isRightIO io = evalIO (attemptEither io) >>= isRight

isLeftIO :: (Show a, Show b) => IO (Either a b) -> PropertyT IO ()
isLeftIO io = evalIO (attemptEither io) >>= isLeft

isRight :: Show a => Either a b -> PropertyT IO ()
isRight (Right _) = success
isRight (Left v)  = annotate ("the value is a Left: " <> show v) >> failure

isLeft :: Show b => Either a b -> PropertyT IO ()
isLeft (Left _)  = success
isLeft (Right v) = annotate ("the value is a Right: " <> show v) >> failure
