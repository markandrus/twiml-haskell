{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Play
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/play TwiML Reference for \<Play\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Play
  ( play
  , play'
  , Play
  , PlayF(..)
  , PlayAttributes
  , HasLoop(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses
import Text.XML.Twiml.Types

play :: IsTwimlLike f Play => URL -> PlayAttributes -> TwimlLike f Play ()
play a b = iliftF . inj $ PlayF (pure a) b ()

play' :: IsTwimlLike f Play => Maybe URL -> PlayAttributes -> TwimlLike f Play ()
play' a b = iliftF . inj $ PlayF a b ()
