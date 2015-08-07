-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- This module re-exports all the TwiML verbs. For more information, refer to
-- Twilio's documentation on
-- <https://www.twilio.com/docs/api/twiml#verbs The TwiML Verbs>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs (module X) where

import Text.XML.Twiml.Verbs.Dial as X
import Text.XML.Twiml.Verbs.End as X
import Text.XML.Twiml.Verbs.Enqueue as X
import Text.XML.Twiml.Verbs.Gather as X
import Text.XML.Twiml.Verbs.Hangup as X
import Text.XML.Twiml.Verbs.Leave as X
import Text.XML.Twiml.Verbs.Message as X
import Text.XML.Twiml.Verbs.Pause as X
import Text.XML.Twiml.Verbs.Play as X
import Text.XML.Twiml.Verbs.Record as X
import Text.XML.Twiml.Verbs.Redirect as X
import Text.XML.Twiml.Verbs.Reject as X
import Text.XML.Twiml.Verbs.Say as X
import Text.XML.Twiml.Verbs.Sms as X
