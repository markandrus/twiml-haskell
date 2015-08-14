-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Text.XML.Twiml
  ( MessagingTwiml(..)
  , VoiceTwiml(..)
  , response
  , module X
  ) where

import Text.XML.Twiml.Types as X
import Text.XML.Twiml.Verbs as X
import Text.XML.Twiml.Internal.Twiml
