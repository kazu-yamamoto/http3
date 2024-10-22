{-# LANGUAGE OverloadedStrings #-}

module ServerX (
    serverHQ,
    serverH3,
)
where

import Control.Monad
import Crypto.Hash (Context, SHA1) -- crypton
import qualified Crypto.Hash as CH
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as C8
import qualified Network.HQ.Server as HQ
import Network.HTTP.Types
import Network.HTTP2.Server hiding (run)
import qualified Network.HTTP3.Server as H3
import qualified Control.Exception as E

import Network.QUIC

serverHQ :: Connection -> IO ()
serverHQ = serverX HQ.run

serverH3 :: Connection -> IO ()
serverH3 = serverX H3.run

serverX
    :: (Connection -> H3.Config -> H3.Server -> IO ()) -> Connection -> IO ()
serverX run conn = E.bracket H3.allocSimpleConfig H3.freeSimpleConfig $ \conf ->
    run conn conf server

server :: Server
server req _aux sendResponse = case requestMethod req of
    Just "GET"
        | requestPath req == Just "/" -> sendResponse responseHello []
    Just "POST" -> sendResponse (responseEcho req) []
    _ -> sendResponse response404 []

responseHello :: Response
responseHello = responseBuilder ok200 header body
  where
    header =
        [ ("Content-Type", "text/plain")
        , ("Server", "HaskellQuic/0.0.0")
        ]
    body = byteString "Hello, world!\n"

response404 :: Response
response404 = responseBuilder notFound404 header body
  where
    header =
        [ ("Content-Type", "text/plain")
        , ("Server", "HaskellQuic/0.0.0")
        ]
    body = byteString "Not found\n"

responseEcho :: Request -> Response
responseEcho req = setResponseTrailersMaker h2rsp maker
  where
    h2rsp = responseStreaming ok200 header streamingBody
    header = [("Content-Type", "text/plain")]
    streamingBody write _flush = loop
      where
        loop = do
            bs <- getRequestBodyChunk req
            unless (B.null bs) $ do
                void $ write $ byteString bs
                loop
    maker = trailersMaker (CH.hashInit :: Context SHA1)

-- Strictness is important for Context.
trailersMaker :: Context SHA1 -> Maybe ByteString -> IO NextTrailersMaker
trailersMaker ctx Nothing = return $ Trailers [("X-SHA1", sha1)]
  where
    sha1 = C8.pack $ show $ CH.hashFinalize ctx
trailersMaker ctx (Just bs) = return $ NextTrailersMaker $ trailersMaker ctx'
  where
    ctx' = CH.hashUpdate ctx bs
