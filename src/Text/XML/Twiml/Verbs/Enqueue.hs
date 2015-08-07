{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Enqueue
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/enqueue TwiML Reference for \<Enqueue\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Enqueue
  ( enqueue
  , Enqueue
  , EnqueueF(..)
  , EnqueueAttributes
  , HasAction(..)
  , HasMethod(..)
  , HasWaitMethod(..)
  , HasWaitURL(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses

enqueue :: IsTwimlLike f Enqueue => String -> EnqueueAttributes -> TwimlLike f Enqueue ()
enqueue a b = iliftF . inj $ EnqueueF a b ()
