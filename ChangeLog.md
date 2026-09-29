# Revision history for http3

## 0.1.6

* Requiring quic v0.3.9 and http2 v5.4.6.  quic v0.3.7 stops opening a
  closed stream again for a late copy of its data, which closed the
  connection with FLOW_CONTROL_ERROR now and then (#15); v0.3.8 tells a
  stream that was reset from one that ended; v0.3.9 keeps the peer's open
  streams within initial_max_streams, closes the sending part on
  STOP_SENDING, and opens a stream for a RESET_STREAM that comes before
  its data.
* QPACK: fixing the dynamic table where the two ends disagreed: the
  maximum number of entries, when a section is blocked, the blocked
  streams, more than one outstanding section on a stream, and a change of
  capacity.  Encoding a field larger than the encoder's buffers.
* QPACK: stopping counting a blocked stream however its wait ends.
* QPACK: evicting an entry nothing refers to once its insertion is
  acknowledged.
* QPACK: sending Stream Cancellation for a stream not read to its end or
  reset by the peer, and acting on one received.
* QPACK: inserting with a reference to a name not yet acknowledged.
* Holding a field section to our SETTINGS_MAX_FIELD_SECTION_SIZE as it
  decodes, not only the frame carrying it; a server answers a request
  over it with 431.  The new `FieldSectionTooLarge` is thrown otherwise.
* Keeping to the peer's SETTINGS_MAX_FIELD_SECTION_SIZE when sending.  The
  new `FieldSectionTooLargeForPeer` is thrown by `sendRequest` and
  `sendResponse` for a header section over it.
* Reading on past a DATA frame that is empty.
* Treating a message with a connection-specific field as malformed.  The
  new `ConnectionSpecificField` is thrown for a response or trailers
  carrying one.
* Refusing a second control or QPACK stream, and noticing a QPACK stream
  closing.  Refusing a push stream.
* Stopping reading a unidirectional stream of an unknown type, and
  refusing a server-initiated bidirectional stream.
* A client whose request the server stops with STOP_SENDING no longer
  resets the stream, so that a response sent after it is still read.
* Closing a unidirectional stream we stop reading, so that it is given
  back to the peer's limit.
* This is a patch release, but `TableOperation` in `Network.QPACK` has a
  new field, `getHeaderSize`.

## 0.1.5

* Security fixes.  Requiring http2 v5.4.5, whose HPACK integer decoder is
  bounded; QPACK decodes its integers with it.
* Refusing a QPACK index that names nothing, at both ends of both tables.
* Putting a ceiling on the frame payload we will hold.
* Sizing the Huffman scratch buffer to what it has to hold.
* Reading a unidirectional stream type as the variable-length integer it is.
* Not handing the application a request we have already rejected.
* Checking a message against the content-length it declared.
* Reading the whole SETTINGS frame, noticing a repeated identifier, and
  refusing one that stops mid-parameter.
* Giving the application the peer's address, not our own.
* This is a patch release, but `Network.HTTP3.Internal` and
  `Network.QPACK.Internal` changed: `parseH3Frame` takes the payload limit,
  `IFrame` has `ITooLong`, and `DecodeError` has `IllegalDynamicIndex`.

## 0.1.4

* adding ecUseHuffman to defaultQEncoderConfig

## 0.1.3

* Using quic v0.3.

## 0.1.2

* Updating dependencies.

## 0.1.1

* Removing a debug logging to stdout.

## 0.1.0

* QPACK encoder now supports the dynamic table.
* Breaking change: `Config` takes 'confQEncoderConfig' and
  `confQDecoderConfig`.

## 0.0.24

* Supporting SSLKEYLOGFILE in h3-server and h3-client.
* Sending SectionAcknowledgement only when reqInsCnt /= 0.

## 0.0.23

* Enclosing IPv6 address in :authority

## 0.0.22

* Using `ThreadManager` of `time-manager`.

## 0.0.21

* Using `withHandle` of `time-manager`.

## 0.0.20

* Unregistering handle to remove ThreadId to prevent temporary
  thread leak.

## 0.0.19

* Labeling threads.
* Removing `unliftio`.
* Using `http-semantics` v0.3.

## 0.0.18

* Using http-semantics v0.2.1

## 0.0.17

* Providing ServerIO API

## 0.0.16

* Using quic v0.2

## 0.0.15

* Using http-semantics v0.2

## 0.0.14

* Preparing for tls v2.1

## 0.0.13

* Using OutBodyIface.
  [#5](https://github.com/kazu-yamamoto/http3/pull/5)

## 0.0.12

* Catching up http-semantics v0.0.1.

## 0.0.11

* Using http-semantics.

## 0.0.10

* Locking QPCK encoder
* Renaming util/{client,server} to util/{h3-client,h3-server}.

## 0.0.9

* Fixing the support for http2 v5.1.

## 0.0.8

* Using http2 v5.1.

## 0.0.7

* Supporting http2 v5.0.

## 0.0.6

* Rescuing GHC 9.0 for testing.

## 0.0.5

* Supporting http2 v4.2.0.

## 0.0.4

* Using "crypton" intead of "cryptonite".

## 0.0.3

* Fixes for HTTP/3 CONNECT proxy
  [#4](https://github.com/kazu-yamamoto/http3/pull/4)

## 0.0.2

* Catching up http2 v4.1.

## 0.0.1

* Supporting QUICv2.

## 0.0.0

* First version. Released on an unsuspecting world.
