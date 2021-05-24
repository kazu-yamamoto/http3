{-# LANGUAGE OverloadedStrings #-}

module HTTP3.Server (
    setup
  , teardown
  , trailersMaker
  , firstTrailerValue
  , CH.hashInit
  ) where

import Control.Concurrent
import Control.Monad
import Crypto.Hash (Context, SHA1) -- cryptonite
import qualified Crypto.Hash as CH
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as C8
import Data.IP ()
import Network.HPACK
import Network.HTTP.Types
import Network.HTTP3.Server
import qualified Network.QUIC as QUIC
import Network.Socket ()
import Test.Hspec
import qualified UnliftIO.Exception as E

import HTTP3.Config

setup :: IO ThreadId
setup = do
    sc <- makeTestServerConfig
    tid <- forkIO $ QUIC.runQUICServer sc loop
    threadDelay 500000 -- give enough time to the server
    return tid
  where
    loop conn = E.bracket allocSimpleConfig freeSimpleConfig $ \conf -> run conn conf server

teardown :: ThreadId -> IO ()
teardown tid = killThread tid

server :: Server
server req _aux sendResponse = case requestMethod req of
  Just "GET"  -> case requestPath req of
                   Just "/" -> sendResponse responseHello []
                   _        -> sendResponse response404 []
  Just "POST" -> case requestPath req of
                   Just "/echo" -> sendResponse (responseEcho req) []
                   _        -> sendResponse responseHello []
  _           -> sendResponse response405 []

responseHello :: Response
responseHello = responseBuilder ok200 header body
  where
    header = [("Content-Type", "text/plain")]
    body = byteString "Hello, world!\n"

response404 :: Response
response404 = responseNoBody notFound404 []

response405 :: Response
response405 = responseNoBody methodNotAllowed405 []

responseEcho :: Request -> Response
responseEcho req = setResponseTrailersMaker h2rsp maker
  where
    h2rsp = responseStreaming ok200 header streamingBody
    header = [("Content-Type", "text/plain")]
    streamingBody write _flush = do
        loop
        mt <- getRequestTrailers req
        firstTrailerValue <$> mt `shouldBe` Just "b0870457df2b8cae06a88657a198d9b52f8e2b0a"
      where
        loop = do
            bs <- getRequestBodyChunk req
            unless (B.null bs) $ do
                void $ write $ byteString bs
                loop
    maker = trailersMaker CH.hashInit

-- Strictness is important for Context.
trailersMaker :: Context SHA1 -> Maybe ByteString -> IO NextTrailersMaker
trailersMaker ctx Nothing = return $ Trailers [("X-SHA1", sha1)]
  where
    sha1 = C8.pack $ show $ CH.hashFinalize ctx
trailersMaker ctx (Just bs) = return $ NextTrailersMaker $ trailersMaker ctx'
  where
    ctx' = CH.hashUpdate ctx bs

firstTrailerValue :: HeaderTable -> HeaderValue
firstTrailerValue = snd . Prelude.head . fst
