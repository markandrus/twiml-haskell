{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Dial
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/dial TwiML Reference for \<Dial\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Dial
  ( dial
  , dial'
  , Dial
  , DialF(..)
  , DialAttributes
  , HasAction(..)
  , HasMethod(..)
  , HasTimeout(..)
  , HasCallerId(..)
  , HasHangupOnStar(..)
  , HasRecord'(..)
  , HasTimeLimit(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses
import Text.XML.Twiml.Types

dial :: IsTwimlLike f Dial => String -> DialAttributes -> TwimlLike f Dial ()
dial a b = iliftF . inj $ DialF (pure a) b ()

dial' :: IsTwimlLike f Dial => Either DialNoun String -> DialAttributes -> TwimlLike f Dial ()
dial' a b = iliftF . inj $ DialF a b ()
