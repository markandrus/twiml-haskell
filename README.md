twiml
=====

[![twiml on Hackage](https://img.shields.io/hackage/v/twiml.svg)](https://hackage.haskell.org/package/twiml) [![twiml-haskell on Travis CI](https://travis-ci.org/markandrus/twiml-haskell.svg)](https://travis-ci.org/markandrus/twiml-haskell)

This package provides a library for constructing
[TwiML](www.twilio.com/docs/api/twiml). Install using

```
$ cabal install twiml
```

Documentation is available through [GitHub](https://markandrus.github.io/twiml-haskell)
(for HEAD) or [Hackage](https://hackage.haskell.org/package/twiml) for the
current and preceding releases.

For the Twilio REST API, see [twilio-haskell](https://github.com/markandrus/twilio-haskell).

Example
-------

The following Haskell code

```hs
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude
import Control.Lens
import Data.Default
import Text.XML.Twiml
import qualified Text.XML.Twiml.Syntax as Twiml

example :: VoiceTwiml
example =
  voiceResponse $ do
    say "Hello, world" $ def & voice .~ Man
    hangup
  where Twiml.Syntax{..} = def
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

Feel free to contribute to any of the open [issues](https://github.com/markandrus/twiml-haskell/issues),
bugfixes, etc. When you think you're ready to merge, ensure the tests are
passing and open a pull request. If you are adding new functionality, please
include new tests as well. Finally, add yourself to the `AUTHORS` file.
