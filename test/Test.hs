{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM_)
import qualified Data.ByteString as B
import qualified Data.Cabinet as C
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

basicInsert :: Test
basicInsert = TestCase $ do
  let contents = "test"

  pool <- C.newPool
  file <- C.buildFile "test" "text/plain" False contents
  uuid <- C.addToPool pool file

  count <- C.poolCount pool
  assertEqual "inserting one results in one" count 1

  received_contents <- C.poolLookup pool uuid
  case received_contents of
    Just (C.IndexEntry name content_type idx_uuid _, received) -> do
      assertEqual "expected name to match." name "test"
      assertEqual "expected content-type match." content_type "text/plain"
      assertEqual "expected UUID to match." idx_uuid uuid
      assertEqual "expected content to match." contents received
    Nothing -> assertFailure "returned UUID doesn't map to any file."

  idx <- C.poolIndex pool
  case idx of
    [C.IndexEntry name content_type idx_uuid _] -> do
      assertEqual "expected name to be unchanged." name "test"
      assertEqual "expected content-type to be unchanged." content_type "text/plain"
      assertEqual "returned UUID doesn't match." idx_uuid uuid
    _ -> assertFailure "Index had the wrong shape."

  return ()

gcCycle :: Test
gcCycle = TestCase $ do
  pool <- C.newPool

  first <- C.buildFile "test" "text/plain" False ""
  firstUuid <- C.addToPool pool first

  replicateM_ 8 $ do
    x <- C.buildFile "test" "text/plain" False ""
    C.addToPool pool x
    return ()

  bigGcTrigger <-
    C.buildFile
      "gc-trigger"
      "text/plain"
      False
      $ B.replicate (1024 * 1024) 65

  C.addToPool pool bigGcTrigger

  idx <- C.poolIndex pool
  assertBool
    "first inserted file should have been GC'd."
    (all (\(C.IndexEntry _ _ uuid _) -> uuid /= firstUuid) idx)

  return ()

main :: IO ()
main =
  runTestTT tests >>= \counts ->
    if errors counts + failures counts == 0 then exitSuccess else exitFailure
  where
    tests =
      TestList [basicInsert, gcCycle]
