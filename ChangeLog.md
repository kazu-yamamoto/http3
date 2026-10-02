# Revision history for http3

## 0.1.7

* Require quic 0.3.10.
  [#41](https://github.com/kazu-yamamoto/http3/pull/41)
* Close the connection with H3_CLOSED_CRITICAL_STREAM when one of our
  QPACK streams is closed.
  [#40](https://github.com/kazu-yamamoto/http3/pull/40)
* QPACK: send Set Dynamic Table Capacity only when the capacity changes.
  `getMaxTableCapacity` is new.
  [#40](https://github.com/kazu-yamamoto/http3/pull/40)
* h3-server: allow more unidirectional streams.
  [#39](https://github.com/kazu-yamamoto/http3/pull/39)
* Tests: `h3ErrorSpec` can run against a server elsewhere.
  [#38](https://github.com/kazu-yamamoto/http3/pull/38)

## 0.1.6

* Require quic 0.3.9 and http2 5.4.6.
  [#21](https://github.com/kazu-yamamoto/http3/pull/21)
  [#30](https://github.com/kazu-yamamoto/http3/pull/30)
  [#37](https://github.com/kazu-yamamoto/http3/pull/37)
* QPACK: fix the dynamic table where the two ends disagreed.
  [#21](https://github.com/kazu-yamamoto/http3/pull/21)
* QPACK: stop counting a blocked stream however its wait ends.
  [#23](https://github.com/kazu-yamamoto/http3/pull/23)
* QPACK: evict an unreferenced entry once its insertion is acknowledged.
  [#24](https://github.com/kazu-yamamoto/http3/pull/24)
* QPACK: send and act on Stream Cancellation.
  [#26](https://github.com/kazu-yamamoto/http3/pull/26)
  [#35](https://github.com/kazu-yamamoto/http3/pull/35)
* QPACK: insert with a reference to a name not yet acknowledged.
  [#32](https://github.com/kazu-yamamoto/http3/pull/32)
* Hold a field section to our SETTINGS_MAX_FIELD_SECTION_SIZE while
  decoding.  `FieldSectionTooLarge` is new.
  [#28](https://github.com/kazu-yamamoto/http3/pull/28)
* Keep to the peer's SETTINGS_MAX_FIELD_SECTION_SIZE when sending.
  `FieldSectionTooLargeForPeer` is new.
  [#33](https://github.com/kazu-yamamoto/http3/pull/33)
* Read on past an empty DATA frame.
  [#22](https://github.com/kazu-yamamoto/http3/pull/22)
* Treat a message with a connection-specific field as malformed.
  `ConnectionSpecificField` is new.
  [#29](https://github.com/kazu-yamamoto/http3/pull/29)
* Refuse a second control or QPACK stream, and a push stream.
  [#25](https://github.com/kazu-yamamoto/http3/pull/25)
* Stop reading a unidirectional stream of an unknown type, and refuse a
  server-initiated bidirectional stream.
  [#31](https://github.com/kazu-yamamoto/http3/pull/31)
* Client: do not reset a stream the server stops with STOP_SENDING.
  [#36](https://github.com/kazu-yamamoto/http3/pull/36)
* Close a unidirectional stream we stop reading.
  [#37](https://github.com/kazu-yamamoto/http3/pull/37)
* `TableOperation` in `Network.QPACK` has a new field, `getHeaderSize`.
  [#33](https://github.com/kazu-yamamoto/http3/pull/33)

## 0.1.5

* Security fixes.  Require http2 5.4.5.
* Refuse a QPACK index that names nothing.
  [#12](https://github.com/kazu-yamamoto/http3/pull/12)
  [#20](https://github.com/kazu-yamamoto/http3/pull/20)
* Limit the frame payload we hold.
  [#13](https://github.com/kazu-yamamoto/http3/pull/13)
* Size the Huffman scratch buffer to what it has to hold.
  [#14](https://github.com/kazu-yamamoto/http3/pull/14)
* Read a unidirectional stream type as a variable-length integer.
  [#16](https://github.com/kazu-yamamoto/http3/pull/16)
* Do not hand the application a request already rejected.
  [#17](https://github.com/kazu-yamamoto/http3/pull/17)
* Check a message against its content-length.
  [#18](https://github.com/kazu-yamamoto/http3/pull/18)
* Read the whole SETTINGS frame and refuse a repeated or truncated one.
  [#19](https://github.com/kazu-yamamoto/http3/pull/19)
  [#20](https://github.com/kazu-yamamoto/http3/pull/20)
* Give the application the peer's address, not our own.
  [#10](https://github.com/kazu-yamamoto/http3/pull/10)
* `Network.HTTP3.Internal` and `Network.QPACK.Internal` changed.

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
