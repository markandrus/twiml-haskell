{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Record
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/record TwiML Reference for \<Record\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Record
  ( record
  , Record
  , RecordF(..)
  , RecordAttributes
  , HasAction(..)
  , HasFinishOnKey(..)
  , HasMethod(..)
  , HasTimeout(..)
  , HasMaxLength(..)
  , HasPlayBeep(..)
  , HasTranscribe(..)
  , HasTranscribeCallback(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses

record :: IsTwimlLike f Record => RecordAttributes -> TwimlLike f Record ()
record a = iliftF . inj $ RecordF a ()
