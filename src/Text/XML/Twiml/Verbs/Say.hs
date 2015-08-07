{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Say
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/say TwiML Reference for \<Say\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Say
  ( say
  , Say
  , SayF(..)
  , SayAttributes
  , HasLoop(..)
  , HasVoice(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses

say :: IsTwimlLike f Say => String -> SayAttributes -> TwimlLike f Say ()
say a b = iliftF . inj $ SayF a b ()
