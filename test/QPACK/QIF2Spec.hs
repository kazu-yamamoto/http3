{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module QPACK.QIF2Spec where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as C8
import Data.CaseInsensitive hiding (map)
import Data.List
import Data.Maybe
import Network.HTTP.Semantics
import Network.HTTP.Semantics.Client.Internal
import Network.HTTP.Semantics.Server
import Network.HTTP.Types (RequestHeaders, notFound404, ok200)
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Server
import qualified Network.HTTP3.Server as S
import qualified Network.QUIC.Client as QUIC
import System.IO
import Test.Hspec

import HTTP3.Config
import HTTP3.Server hiding (server)
import QIF

qiffile :: FilePath
qiffile = "qifs/qifs/fb-req-hq.qif"

spec :: Spec
spec = do
    describe "QIF server" $ do
        it "handles dynamic table of 0-bytes" $ do
            let size = 0
            E.bracket (setup server size) teardown $
                \_ -> runClient size
    describe "QIF server" $ do
        it "handles dynamic table of 256-bytes" $ do
            let size = 256
            E.bracket (setup server size) teardown $
                \_ -> runClient size
    describe "QIF server" $ do
        it "handles dynamic table of 512-bytes" $ do
            let size = 512
            E.bracket (setup server size) teardown $
                \_ -> runClient size
    describe "QIF server" $ do
        it "handles dynamic table of 4096-bytes" $ do
            let size = 4096
            E.bracket (setup server size) teardown $
                \_ -> runClient size

runClient :: Int -> IO ()
runClient siz = QUIC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf0 -> do
        let conf =
                conf0
                    { S.confQEncoderConfig = S.defaultQEncoderConfig{S.ecMaxTableCapacity = siz}
                    , S.confQDecoderConfig = S.defaultQDecoderConfig{S.dcMaxTableCapacity = siz}
                    }

        withFile qiffile ReadMode $ \hdl ->
            C.run conn testH3ClientConfig conf $ client hdl

client :: Handle -> (C.Request -> (C.Response -> IO ()) -> IO a) -> p -> IO ()
client hdl sendRequest _aux = do
    -- delay for set table capacity
    threadDelay 10000
    ms <- forkClient 0 id
    mapM_ takeMVar ms
  where
    forkClient n build = do
        hs <- headerlist hdl
        if null hs
            then return $ build []
            else do
                var <- newEmptyMVar
                _ <- forkIO $ do
                    let req = requestBuilder2 hs mempty
                    _ <- sendRequest req $ checkHeader hs
                    putMVar var ()
                when ((n :: Int) `mod` 50 == 0) $ threadDelay 100000
                forkClient (n + 1) (build . (var :))
    checkHeader hs rsp = do
        bs <- getBody
        let hs0 = map keyValue $ split bs
            hs0' = sort $ filter (\(k, _) -> k == "cookie:") hs0
            hs' = sort $ filter (\(k, _) -> k == "cookie:") hs
        hs0' `shouldBe` hs'
      where
        getBody = B.concat <$> loop id
        loop build = do
            bs <- C.getResponseBodyChunk rsp
            if bs == ""
                then return $ build []
                else loop (build . (bs :))
        split ls0 = go ls0 id
          where
            go "" build = build []
            go ls build = go ls'' (build . (l :))
              where
                (l, ls') = C8.break (== '\n') ls
                ls'' = C8.drop 1 ls'
        keyValue kv = (mk k, v)
          where
            (k, v') = C8.break (== ' ') kv
            v = C8.drop 1 v'

server :: Server
server req _aux sendResponse = case requestMethod req of
    Just "GET" -> sendResponse (responseSection req) []
    _ -> sendResponse (responseNoBody notFound404 []) []

responseSection :: S.Request -> S.Response
responseSection req = responseBuilder ok200 header body
  where
    header =
        [ ("Content-Type", "text/plain")
        , ("Server", "HaskellQuic/0.0.0")
        ]
    (thl, vt) = requestHeaders req
    pseudos = [tokenAuthority, tokenMethod, tokenPath, tokenScheme]
    pthl = map (\t -> (t, fromJust (getFieldValue t vt))) pseudos
    body =
        foldr1 (<>) $
            map (\(k, v) -> byteString (k <> " " <> v <> "\n")) $
                map (\(k, v) -> (tokenFoldedKey k, v)) (pthl ++ thl)

-- | Creating request with builder only from headers
--   where :path and :method are included.
requestBuilder2 :: RequestHeaders -> Builder -> C.Request
requestBuilder2 hdr builder = Request $ OutObj hdr (OutBodyBuilder builder) defaultTrailersMaker
