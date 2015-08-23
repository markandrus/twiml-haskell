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
  , Gather
  , GatherF
  , GatherAttributes
  ) where

import Data.Void
import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml

{- | Example:

#include "gatherExample2.txt"
-}
gather :: (IsTwimlLike f Gather, Nest i In Gather) => GatherAttributes -> TwimlLike' VoiceVerbsF i Void -> TwimlLike f Gather ()
gather a b = iliftF . inj $ GatherF a b ()
