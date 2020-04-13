{-# LANGUAGE OverloadedStrings #-}

module Common (
    getGroups
  , getLogger
  , getDirLogger
  , getStdoutLogger
  , makeProtos
  , toT
  , name
  ) where

import Control.Concurrent
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as C8
import Data.CaseInsensitive hiding (map)
import Data.Default.Class
import Data.Maybe
import Network.HPACK (TokenHeader, HeaderValue)
import Network.HPACK.Token
import Network.HTTP.Types
import Network.TLS hiding (Version)
import Text.Printf
import System.FilePath

import Network.QUIC

namedGroups :: [(String, Group)]
namedGroups =
    [ ("ffdhe2048", FFDHE2048)
    , ("ffdhe3072", FFDHE3072)
    , ("ffdhe4096", FFDHE4096)
    , ("ffdhe6144", FFDHE6144)
    , ("ffdhe8192", FFDHE8192)
    , ("p256",      P256)
    , ("p384",      P384)
    , ("p521",      P521)
    , ("x25519",    X25519)
    , ("x448",      X448)
    ]

getGroups :: Maybe String -> [Group]
getGroups Nothing   = supportedGroups def
getGroups (Just gs) = catMaybes $ map (`lookup` namedGroups) $ split ',' gs

split :: Char -> String -> [String]
split _ "" = []
split c s = case break (c==) s of
    ("",r)  -> split c (tail r)
    (s',"") -> [s']
    (s',r)  -> s' : split c (tail r)

getLogger :: Maybe FilePath -> (String -> IO ())
getLogger Nothing     = \_ -> return ()
getLogger (Just file) = \msg -> appendFile file (msg ++ "\n")

getStdoutLogger :: Bool -> (CID -> String -> IO ())
getStdoutLogger False = \_ _ -> return ()
getStdoutLogger True  = \_ msg -> putStr msg

getDirLogger :: Maybe FilePath -> String -> (CID -> String -> IO ())
getDirLogger Nothing    _      = \_ _ -> return ()
getDirLogger (Just dir) suffix = \cid msg -> do
    let filename = C8.unpack (encode (fromCID cid)) ++ suffix
        logfile = dir </> filename
    appendFile logfile msg `E.catch` \(E.SomeException _) -> do
        threadDelay 1000
        appendFile logfile msg

makeProtos :: Version -> (ByteString, ByteString)
makeProtos ver = (h3X,hqX)
  where
    verbs = C8.pack $ printf "%d" $ fromVersion ver
    h3X = "h3-" `BS.append` verbs
    hqX = "hq-" `BS.append` verbs

toT :: (HeaderName, HeaderValue) -> TokenHeader
toT (k,v) = (toToken $ foldedCase k, v)

name :: ByteString
name = "HaskellQuic/0.0.0"
