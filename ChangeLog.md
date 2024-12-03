# Revision history for http3

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
