{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Record
-- Copyright   :  (C) 2018 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- The example in this file assumes
--
-- @
-- {-\# LANGUAGE RebindableSyntax \#-}
-- {-\# LANGUAGE RecordWildCards \#-}
-- 
-- import Prelude
-- import Control.Lens
-- import Data.Default
-- import Text.XML.Twiml
-- import qualified Text.XML.Twiml.Syntax as Twiml
-- @
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/record TwiML Reference for \<Record\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Record
  ( record
  , Record
  , RecordF
  , RecordAttributes
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml

-- $setup
-- >>> :set -XRebindableSyntax
-- >>> :set -XRecordWildCards
-- >>> import Prelude
-- >>> import Control.Lens
-- >>> import Data.Default
-- >>> import Text.XML.Twiml
-- >>> import qualified Text.XML.Twiml.Syntax as Twiml

{- | Example:

>>> :{
let example :: VoiceTwiml
    example =
      voiceResponse $ do
        say "Please leave a message at the beep. Press the star key when finished." def
        record $ def & action      .~ parseURL "http://foo.edu/handleRecording.php"
                     & method      .~ Just GET
                     & maxLength   .~ Just 20
                     & finishOnKey .~ Just KStar
        say "I did not receive a recording" def
        end
      where Twiml.Syntax{..} = def
:}

>>> putStr $ show example
<?xml version="1.0" encoding="UTF-8"?>
<Response>
  <Say>Please leave a message at the beep. Press the star key when finished.</Say>
  <Record action="http://foo.edu/handleRecording.php" method="GET" finishOnKey="*" maxLength="20" />
  <Say>I did not receive a recording</Say>
</Response>
-}
record :: IsTwimlLike f Record => RecordAttributes -> TwimlLike f Record ()
record a = iliftF . inj $ RecordF a ()
