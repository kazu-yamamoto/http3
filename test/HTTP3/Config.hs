{-# LANGUAGE OverloadedStrings #-}

module HTTP3.Config (
    makeTestServerConfig
  , testClientConfig
  , testH3ClientConfig
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as L
import qualified Network.HTTP3.Client as H3
import Network.TLS (Credentials(..), credentialLoadX509)

import Network.QUIC

makeTestServerConfig :: IO ServerConfig
makeTestServerConfig = do
    cred <- either error id <$> credentialLoadX509 "test/servercert.pem" "test/serverkey.pem"
    let credentials = Credentials [cred]
    return testServerConfig {
        scConfig = (scConfig testServerConfig) {
              confCredentials = credentials
            , confQLog = Just "/Users/kazu/tmp/qlog/"
            }
      , scALPN = Just chooseALPN
      }

testServerConfig :: ServerConfig
testServerConfig = defaultServerConfig {
    scAddresses = [("127.0.0.1",8003)]
  }

testClientConfig :: ClientConfig
testClientConfig = defaultClientConfig {
    ccPortName = "8003"
  , ccConfig = defaultConfig {
        confKeyLog = \msg -> appendFile "/Users/kazu/tls_key.log" (msg ++ "\n")
      }
  }

chooseALPN :: Version -> [ByteString] -> IO ByteString
chooseALPN ver protos = return $ case mh3idx of
    Nothing    -> case mhqidx of
      Nothing    -> ""
      Just _     -> hqX
    Just h3idx ->  case mhqidx of
      Nothing    -> h3X
      Just hqidx -> if h3idx < hqidx then h3X else hqX
  where
    (h3X, hqX) = makeProtos ver
    mh3idx = h3X `L.elemIndex` protos
    mhqidx = hqX `L.elemIndex` protos

makeProtos :: Version -> (ByteString, ByteString)
makeProtos ver = (h3X,hqX)
  where
    verbs = C8.pack $ show $ fromVersion ver
    h3X = "h3-" `BS.append` verbs
    hqX = "hq-" `BS.append` verbs

testH3ClientConfig :: H3.ClientConfig
testH3ClientConfig = H3.ClientConfig "https" "127.0.0.1"
