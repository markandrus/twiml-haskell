twiml
=====

[![twiml-haskell on Travis CI](https://travis-ci.org/markandrus/twiml-haskell.svg)](https://travis-ci.org/markandrus/twiml-haskell)

This package provides a library for constructing
[TwiML](www.twilio.com/docs/api/twiml). Install using

```
$ cabal install twiml
```

Documentation soon to be available on Hackage. For now, see [markandrus.github.io/twiml-haskell](http://markandrus.github.io/twiml-haskell).

Example
-------

The following Haskell code

```hs
example
  = respond
  . sayMan "Hello, world"
  $ hangup
```

is transformed into

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Response>
  <Say voice="man">Hello, world</Say>
  <Hangup/>
</Response>
```

TODO
----

* Implement phone number parsing for `Dial`.
