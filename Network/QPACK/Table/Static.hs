{-# LANGUAGE OverloadedStrings #-}

module Network.QPACK.Table.Static (
    toStaticEntry
  , staticTableSize
  , staticTableList
  ) where

import Data.Array (Array, listArray)
import Data.Array.Base (unsafeAt)
import Network.HPACK.Internal

----------------------------------------------------------------

-- | The size of static table.
staticTableSize :: Size
staticTableSize = length staticTableList

{-# INLINE toStaticEntry #-}
-- | Get 'Entry' from the static table.
--
-- >>> toStaticEntry 1
-- Entry 42 (Token {ix = 0, shouldBeIndexed = True, isPseudo = True, tokenKey = ":authority"}) ""
-- >>> toStaticEntry 8
-- Entry 36 (Token {ix = 24, shouldBeIndexed = False, isPseudo = False, tokenKey = "Etag"}) ""
-- >>> > toStaticEntry 50
-- Entry 52 (Token {ix = 21, shouldBeIndexed = True, isPseudo = False, tokenKey = "Content-Type"}) "text/css"

toStaticEntry :: Index -> Entry
toStaticEntry sidx = staticTable `unsafeAt` (sidx - 1)

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
  , ("accept-ranges", "bytes")
  , ("access-control-allow-headers", "cache-control")
  , ("access-control-allow-headers", "content-type")
  , ("access-control-allow-origin", "*")
  , ("cache-control", "max-age=0")
  , ("cache-control", "max-age=2592000")
  , ("cache-control", "max-age=604800")
  , ("cache-control", "no-cache")
  , ("cache-control", "no-store")
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
  , ("content-type", "text/plain")
  , ("content-type", "text/plain;charset=utf-8")
  , ("range", "bytes=0-")
  , ("strict-transport-security", "max-age=31536000")
  , ("vary", "accept-encoding")
  , ("vary", "origin")
  , ("x-content-type-options", "nosniff")
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
  , ("access-control-allow-methods", "options")
  , ("access-control-expose-headers", "content-length")
  , ("access-control-request-headers", "content-type")
  , ("access-control-request-method", "get")
  , ("access-control-request-method", "post")
  , ("alt-svc", "clear")
  , ("authorization", "")
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
