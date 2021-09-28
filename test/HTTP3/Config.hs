{-# LANGUAGE OverloadedStrings #-}

module HTTP3.Config (
    makeTestServerConfig
  , testClientConfig
  , testH3ClientConfig
  ) where

import Data.ByteString (ByteString)
import qualified Data.List as L
import qualified Network.HTTP3.Client as H3
import Network.TLS (Credentials(..), credentialLoadX509)

import Network.QUIC.Client
import Network.QUIC.Internal

makeTestServerConfig :: IO ServerConfig
makeTestServerConfig = do
    cred <- either error id <$> credentialLoadX509 "test/servercert.pem" "test/serverkey.pem"
    let credentials = Credentials [cred]
    return testServerConfig {
        scCredentials = credentials
      , scALPN = Just chooseALPN
      }

testServerConfig :: ServerConfig
testServerConfig = defaultServerConfig {
    scAddresses = [("127.0.0.1",8003)]
  }

testClientConfig :: ClientConfig
testClientConfig = defaultClientConfig {
    ccPortName = "8003"
  , ccValidate = False
  }

chooseALPN :: Version -> [ByteString] -> IO ByteString
chooseALPN _ver protos = return $ case mh3idx of
    Nothing    -> case mhqidx of
      Nothing    -> ""
      Just _     -> "hq"
    Just h3idx ->  case mhqidx of
      Nothing    -> "h3"
      Just hqidx -> if h3idx < hqidx then "h3" else "hq"
  where
    mh3idx = "h3" `L.elemIndex` protos
    mhqidx = "hq" `L.elemIndex` protos

testH3ClientConfig :: H3.ClientConfig
testH3ClientConfig = H3.ClientConfig "https" "127.0.0.1"
