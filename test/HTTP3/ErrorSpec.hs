{-# LANGUAGE OverloadedStrings #-}

module HTTP3.ErrorSpec where

import Data.ByteString ()
import Test.Hspec

import HTTP3.Config
import HTTP3.Error
import HTTP3.Server

spec :: Spec
spec = beforeAll setup $ afterAll teardown $ h3ErrorSpec testClientConfig testH3ClientConfig 2000 -- 2 seconds
