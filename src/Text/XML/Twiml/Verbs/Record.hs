{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Record
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
-- <https://www.twilio.com/docs/api/twiml/record TwiML Reference for \<Record\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Record
  ( record
  , Record
  , RecordF
  , RecordAttributes
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml

{- | Example:

#include "recordExample2.txt"
-}
record :: IsTwimlLike f Record => RecordAttributes -> TwimlLike f Record ()
record a = iliftF . inj $ RecordF a ()
