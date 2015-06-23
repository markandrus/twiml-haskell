twiml
=====

[![twiml on Hackage](https://img.shields.io/hackage/v/twiml.svg)](https://hackage.haskell.org/package/twiml) [![twiml-haskell on Travis CI](https://travis-ci.org/markandrus/twiml-haskell.svg)](https://travis-ci.org/markandrus/twiml-haskell)

This package provides a library for constructing
[TwiML](www.twilio.com/docs/api/twiml). Install using

```
$ cabal install twiml
```

Documentation soon to be available on Hackage. For now, see [markandrus.github.io/twiml-haskell](http://markandrus.github.io/twiml-haskell).

For the Twilio REST API, see [twilio-haskell](//github.com/markandrus/twilio-haskell).

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

Contributing
------------

Feel free to contribute to any of the open [issues]
(https://github.com/markandrus/twiml-haskell/issues), bugfixes, etc. When you
think you're ready to merge, ensure the tests are passing and open a pull
request. If you are adding new functionality, please include new tests as well.
Finally, add yourself to the `AUTHORS` file.
