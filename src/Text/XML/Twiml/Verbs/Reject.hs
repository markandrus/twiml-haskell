{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Reject
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/reject TwiML Reference for \<Reject\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Reject
  ( reject
  , Reject
  , RejectF(..)
  , RejectAttributes
  , HasReason(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses

reject :: IsTwimlLike f Reject => RejectAttributes -> TwimlLike f Reject a
reject a = iliftF . inj $ RejectF a
