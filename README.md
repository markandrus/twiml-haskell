Twiml
=====

This package provides a library for constructing
[TwiML](www.twilio.com/docs/api/twiml). Install using

```
cabal install twiml
```

Example
-------

The follow Haskell code

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

Update the `Show` instance for `Response` to actually print the attributes of
each TwiML verb.
