{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Pause
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/pause TwiML Reference for \<Pause\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Pause
  ( pause
  , Pause
  , PauseF(..)
  , PauseAttributes
  , HasDuration(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses

pause :: IsTwimlLike f Pause => PauseAttributes -> TwimlLike f Pause ()
pause a = iliftF . inj $ PauseF a ()
