{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Hangup
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/hangup TwiML Reference for \<Hangup\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Hangup
  ( hangup
  , Hangup
  , HangupF(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml

hangup :: IsTwimlLike f Hangup => TwimlLike f Hangup a
hangup = iliftF . inj $ HangupF
