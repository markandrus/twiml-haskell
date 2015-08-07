{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Message
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/sms/message TwiML Reference for \<Message\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Message
  ( message
  , Message
  , MessageF(..)
  , MessageAttributes
  , HasAction(..)
  , HasMethod(..)
  , HasFrom(..)
  , HasStatusCallback(..)
  , HasTo(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses

message :: IsTwimlLike f Message => String -> MessageAttributes -> TwimlLike f Message ()
message a b = iliftF . inj $ MessageF a b ()
