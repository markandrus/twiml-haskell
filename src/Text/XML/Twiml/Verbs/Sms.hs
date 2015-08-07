{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Sms
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/sms TwiML Reference for \<Sms\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Sms
  ( sms
  , Sms
  , SmsF(..)
  , SmsAttributes
  , HasAction(..)
  , HasMethod(..)
  , HasFrom(..)
  , HasStatusCallback(..)
  , HasTo(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses

sms :: IsTwimlLike f Sms => String -> SmsAttributes -> TwimlLike f Sms ()
sms a b = iliftF . inj $ SmsF a b ()
