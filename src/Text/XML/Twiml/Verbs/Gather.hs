{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Gather
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- The example in this file assumes
--
-- @
-- {-\# LANGUAGE RebindableSyntax \#-}
-- {-\# LANGUAGE RecordWildCards \#-}
-- 
-- import Prelude
-- import Text.XML.Twiml
-- import qualified Text.XML.Twiml.Syntax as Twiml
-- @
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/gather TwiML Reference for \<Gather\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Gather
  ( gather
    -- * Data Types
  , Gather
  , GatherF(..)
    -- ** Attributes
  , GatherAttributes
    -- * Attribute Lenses
  , HasAction(..)
  , HasFinishOnKey(..)
  , HasMethod(..)
  , HasNumDigits(..)
  , HasTimeout(..)
  ) where

import Data.Void
import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses

{- | Example:

#include "gatherExample2.txt"
-}
gather :: (IsTwimlLike f Gather, Nest i In Gather) => GatherAttributes -> TwimlLike' VoiceTwimlF i Void -> TwimlLike f Gather ()
gather a b = iliftF . inj $ GatherF a b ()
