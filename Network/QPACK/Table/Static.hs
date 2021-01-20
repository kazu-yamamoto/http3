{-# LANGUAGE OverloadedStrings #-}

module Network.QPACK.Table.Static (
    toStaticEntry
  , staticTableSize
  , staticTableList
  ) where

import qualified Control.Exception as E
import Data.Array (Array, listArray)
import Data.Array.Base (unsafeAt)
import Network.HPACK.Internal

import Network.QPACK.Error
import Network.QPACK.Types

----------------------------------------------------------------

-- | The size of static table.
staticTableSize :: Size
staticTableSize = length staticTableList

{-# INLINE toStaticEntry #-}
-- | Get 'Entry' from the static table.
--
-- >>> toStaticEntry 1
-- Entry 38 (Token {tokenIx = 2, shouldBeIndexed = False, isPseudo = True, tokenKey = ":path"}) "/"
-- >>> toStaticEntry 8
-- Entry 49 (Token {tokenIx = 30, shouldBeIndexed = True, isPseudo = False, tokenKey = "If-Modified-Since"}) ""
-- >>> toStaticEntry 50
-- Entry 53 (Token {tokenIx = 21, shouldBeIndexed = True, isPseudo = False, tokenKey = "Content-Type"}) "image/png"
toStaticEntry :: AbsoluteIndex -> Entry
toStaticEntry (AbsoluteIndex sidx)
  | sidx < staticTableSize = staticTable `unsafeAt` sidx
  | otherwise              = E.throw IllegalStaticIndex

-- | Pre-defined static table.
staticTable :: Array Index Entry
staticTable = listArray (1,staticTableSize) $ map toEntry staticTableList

----------------------------------------------------------------

staticTableList :: [Header]
staticTableList = [
    (":authority", "")
  , (":path", "/")
  , ("age", "0")
  , ("content-disposition", "")
  , ("content-length", "0")
  , ("cookie", "")
  , ("date", "")
  , ("etag", "")
  , ("if-modified-since", "")
  , ("if-none-match", "")
  , ("last-modified", "")
  , ("link", "")
  , ("location", "")
  , ("referer", "")
  , ("set-cookie", "")
  , (":method", "CONNECT")
  , (":method", "DELETE")
  , (":method", "GET")
  , (":method", "HEAD")
  , (":method", "OPTIONS")
  , (":method", "POST")
  , (":method", "PUT")
  , (":scheme", "http")
  , (":scheme", "https")
  , (":status", "103")
  , (":status", "200")
  , (":status", "304")
  , (":status", "404")
  , (":status", "503")
  , ("accept", "*/*")
  , ("accept", "application/dns-message")
  , ("accept-encoding", "gzip, deflate, br")
  , ("accept-ranges", "bytes")
  , ("access-control-allow-headers", "cache-control")
  , ("access-control-allow-headers", "content-type")
  , ("access-control-allow-origin", "*")
  , ("cache-control", "max-age=0")
  , ("cache-control", "max-age=2592000")
  , ("cache-control", "max-age=604800")
  , ("cache-control", "no-cache")
  , ("cache-control", "no-store")
  , ("cache-control", "public, age=31536000")
  , ("content-encoding", "br")
  , ("content-encoding", "gzip")
  , ("content-type", "application/dns-message")
  , ("content-type", "application/javascript")
  , ("content-type", "application/json")
  , ("content-type", "application/x-www-form-urlencoded")
  , ("content-type", "image/gif")
  , ("content-type", "image/jpeg")
  , ("content-type", "image/png")
  , ("content-type", "text/css")
  , ("content-type", "text/html; charset=utf-8")
  , ("content-type", "text/plain")
  , ("content-type", "text/plain;charset=utf-8")
  , ("range", "bytes=0-")
  , ("strict-transport-security", "max-age=31536000")
  , ("strict-transport-security", "max-age=31536000; includesubdomains")
  , ("strict-transport-security", "max-age=31536000; includesubdomains; preload")
  , ("vary", "accept-encoding")
  , ("vary", "origin")
  , ("x-content-type-options", "nosniff")
  , ("x-xss-protection", "1; mode=block")
  , (":status", "100")
  , (":status", "204")
  , (":status", "206")
  , (":status", "302")
  , (":status", "400")
  , (":status", "403")
  , (":status", "421")
  , (":status", "425")
  , (":status", "500")
  , ("accept-language", "")
  , ("access-control-allow-credentials", "FALSE")
  , ("access-control-allow-credentials", "TRUE")
  , ("access-control-allow-headers", "*")
  , ("access-control-allow-methods", "get")
  , ("access-control-allow-methods", "get, post, options")
  , ("access-control-allow-methods", "options")
  , ("access-control-expose-headers", "content-length")
  , ("access-control-request-headers", "content-type")
  , ("access-control-request-method", "get")
  , ("access-control-request-method", "post")
  , ("alt-svc", "clear")
  , ("authorization", "")
  , ("content-security-policy", "script-src 'none'; object-src 'none'; base-uri 'none'")
  , ("early-data", "1")
  , ("expect-ct", "")
  , ("forwarded", "")
  , ("if-range", "")
  , ("origin", "")
  , ("purpose", "prefetch")
  , ("server", "")
  , ("timing-allow-origin", "*")
  , ("upgrade-insecure-requests", "1")
  , ("user-agent", "")
  , ("x-forwarded-for", "")
  , ("x-frame-options", "deny")
  , ("x-frame-options", "sameorigin")
  ]
