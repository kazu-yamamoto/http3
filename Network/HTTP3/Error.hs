{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Network.HTTP3.Error (
    ContentLengthMismatch (..),
    ConnectionSpecificField (..),
    InvalidContentLength (..),
    StreamResetIsReceived (..),
    StreamResetIsSent (..),
    UnsupportedRequest (..),
    StreamTerminated (..),
    toHTTPError,
    translating,
    untranslating,
    ApplicationProtocolError (
        H3NoError,
        H3GeneralProtocolError,
        H3InternalError,
        H3StreamCreationError,
        H3ClosedCriticalStream,
        H3FrameUnexpected,
        H3FrameError,
        H3ExcessiveLoad,
        H3IdError,
        H3SettingsError,
        H3MissingSettings,
        H3RequestRejected,
        H3RequestCancelled,
        H3RequestIncomplete,
        H3MessageError,
        H3ConnectError,
        H3VersionFallback
    ),
) where

import qualified Control.Exception as E
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Network.HTTP.Semantics (StreamTerminated (..))
import qualified Network.HTTP.Semantics.Error as HE
import Network.QUIC

-- | A message whose content does not match the content-length it declared.
--
-- RFC 9114 section 4.1.2 makes such a message malformed. The body reader
-- raises this rather than resetting anything itself: it has no stream to hand,
-- and the handler that wraps the application turns it into the
-- H3_MESSAGE_ERROR reset the spec asks for -- which also stops the application
-- working on a message the receiver has already rejected.
data ContentLengthMismatch = ContentLengthMismatch
    { declaredLength :: Int
    , actualLength :: Int
    }
    deriving (Eq, Show)

instance E.Exception ContentLengthMismatch

-- | A message carrying a connection-specific field, named here, which makes
--   it malformed (RFC 9114, section 4.2).  Thrown where 'ContentLengthMismatch'
--   is, and handled the same way.
newtype ConnectionSpecificField = ConnectionSpecificField ByteString
    deriving (Eq, Show)

instance E.Exception ConnectionSpecificField

-- | A message whose content-length, given here, is not a number of octets,
--   which makes it malformed (RFC 9114, section 4.1.2).
--
--   @since 0.1.12
newtype InvalidContentLength = InvalidContentLength ByteString
    deriving (Eq, Show)

instance E.Exception InvalidContentLength

-- | The peer reset the stream, with this error code, before the body came to
--   its end: what has been read of the body is not the whole of it.
--
-- quic tells a reset to the reader as the end of the stream, and the body
-- reader used to take it for the end of the body.
newtype StreamResetIsReceived = StreamResetIsReceived ApplicationProtocolError
    deriving (Eq, Show)

instance E.Exception StreamResetIsReceived

-- | We reset the stream with this error code: the peer's message on it was
--   not one we could take.  A stream error, unlike 'ApplicationProtocolErrorIsSent',
--   which says the connection was closed.
--
--   @since 0.1.12
newtype StreamResetIsSent = StreamResetIsSent ApplicationProtocolError
    deriving (Eq, Show)

instance E.Exception StreamResetIsSent

-- | A request HTTP/0.9 cannot carry: a method other than GET, or no path.
--   The HQ client throws it before opening a stream.
--
--   @since 0.1.12
newtype UnsupportedRequest = UnsupportedRequest String
    deriving (Eq, Show)

instance E.Exception UnsupportedRequest

{- FOURMOLU_DISABLE -}
pattern H3NoError                :: ApplicationProtocolError
pattern H3NoError                 = ApplicationProtocolError 0x100

pattern H3GeneralProtocolError   :: ApplicationProtocolError
pattern H3GeneralProtocolError    = ApplicationProtocolError 0x101

pattern H3InternalError          :: ApplicationProtocolError
pattern H3InternalError           = ApplicationProtocolError 0x102

pattern H3StreamCreationError    :: ApplicationProtocolError
pattern H3StreamCreationError     = ApplicationProtocolError 0x103

pattern H3ClosedCriticalStream   :: ApplicationProtocolError
pattern H3ClosedCriticalStream    = ApplicationProtocolError 0x104

pattern H3FrameUnexpected        :: ApplicationProtocolError
pattern H3FrameUnexpected         = ApplicationProtocolError 0x105

pattern H3FrameError             :: ApplicationProtocolError
pattern H3FrameError              = ApplicationProtocolError 0x106

pattern H3ExcessiveLoad          :: ApplicationProtocolError
pattern H3ExcessiveLoad           = ApplicationProtocolError 0x107

pattern H3IdError                :: ApplicationProtocolError
pattern H3IdError                 = ApplicationProtocolError 0x108

pattern H3SettingsError          :: ApplicationProtocolError
pattern H3SettingsError           = ApplicationProtocolError 0x109

pattern H3MissingSettings        :: ApplicationProtocolError
pattern H3MissingSettings         = ApplicationProtocolError 0x10a

pattern H3RequestRejected        :: ApplicationProtocolError
pattern H3RequestRejected         = ApplicationProtocolError 0x10b

pattern H3RequestCancelled       :: ApplicationProtocolError
pattern H3RequestCancelled        = ApplicationProtocolError 0x10c

pattern H3RequestIncomplete      :: ApplicationProtocolError
pattern H3RequestIncomplete       = ApplicationProtocolError 0x10d

pattern H3MessageError           :: ApplicationProtocolError
pattern H3MessageError            = ApplicationProtocolError 0x10e

pattern H3ConnectError           :: ApplicationProtocolError
pattern H3ConnectError            = ApplicationProtocolError 0x10f

pattern H3VersionFallback        :: ApplicationProtocolError
pattern H3VersionFallback         = ApplicationProtocolError 0x110
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

-- | What an exception of quic's or of this library is as http-semantics'
--   'HE.HTTPError', the same as HTTP\/2's, given the stream, if any, it
--   happened on.  Others, giving up among them, are as they are.
toHTTPError :: Maybe StreamId -> E.SomeException -> Maybe HE.HTTPError
toHTTPError msid se
    | Just e <- E.fromException se = Just e
    | Just (StreamResetIsReceived c) <- E.fromException se
    , Just sid <- msid =
        Just $ HE.StreamErrorIsReceived (h3 c) sid
    | Just (StreamResetIsSent c) <- E.fromException se
    , Just sid <- msid =
        Just $ HE.StreamErrorIsSent (h3 c) sid ""
    | malformed
    , Just sid <- msid =
        Just $ HE.StreamErrorIsSent (h3 H3MessageError) sid "malformed"
    | Just qe <- E.fromException se = case qe of
        ConnectionIsTimeout _ -> Just HE.ConnectionIsTimeout
        ConnectionIsClosed _ -> Just HE.ConnectionIsClosed
        ConnectionIsReset -> Just HE.ConnectionIsReset
        ApplicationProtocolErrorIsReceived c msg ->
            Just $ HE.ConnectionErrorIsReceived (h3 c) msg
        ApplicationProtocolErrorIsSent c msg ->
            Just $ HE.ConnectionErrorIsSent (h3 c) msg
        TransportErrorIsReceived c msg ->
            Just $ HE.ConnectionErrorIsReceived (transport c) msg
        TransportErrorIsSent c msg ->
            Just $ HE.ConnectionErrorIsSent (transport c) msg
        _ -> Nothing
    | otherwise = Nothing
  where
    h3 (ApplicationProtocolError c) = HE.http3ErrorCode $ fromIntegral c
    transport (TransportError c) = HE.quicTransportErrorCode $ fromIntegral c
    malformed =
        isJust (E.fromException se :: Maybe ContentLengthMismatch)
            || isJust (E.fromException se :: Maybe ConnectionSpecificField)
            || isJust (E.fromException se :: Maybe InvalidContentLength)

-- | Throwing what the action throws as 'HE.HTTPError', where it is one.
translating :: Maybe StreamId -> IO a -> IO a
translating msid = E.handle $ \se -> E.throwIO $ maybe se E.toException $ toHTTPError msid se

-- | The other way round, for what goes on to quic, which knows its own
--   exceptions: a connection the client was told is closed is closed for
--   quic, not an application failing.
untranslating :: IO a -> IO a
untranslating = E.handle $ \se -> E.throwIO $ maybe se id $ fromHTTPError se
  where
    fromHTTPError se = case E.fromException se of
        Just HE.ConnectionIsTimeout -> Just $ E.toException $ ConnectionIsTimeout ""
        Just HE.ConnectionIsClosed -> Just $ E.toException $ ConnectionIsClosed ""
        Just HE.ConnectionIsReset -> Just $ E.toException ConnectionIsReset
        Just (HE.ConnectionErrorIsReceived c msg) -> Just $ E.toException $ case HE.errorSpace c of
            HE.QUICTransport -> TransportErrorIsReceived (TransportError $ value c) msg
            _ -> ApplicationProtocolErrorIsReceived (ApplicationProtocolError $ value c) msg
        Just (HE.ConnectionErrorIsSent c msg) -> Just $ E.toException $ case HE.errorSpace c of
            HE.QUICTransport -> TransportErrorIsSent (TransportError $ value c) msg
            _ -> ApplicationProtocolErrorIsSent (ApplicationProtocolError $ value c) msg
        _ -> Nothing
    value = fromIntegral . HE.errorValue
