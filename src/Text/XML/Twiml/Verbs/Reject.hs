{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Reject
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
-- <https://www.twilio.com/docs/api/twiml/reject TwiML Reference for \<Reject\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Reject
  ( reject
    -- * Data Types
  , Reject
  , RejectF(..)
    -- ** Attributes
  , RejectAttributes
    -- * Attribute Lenses
  , HasReason(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses

{- | Example:

#include "rejectExample2.txt"
-}
reject :: IsTwimlLike f Reject => RejectAttributes -> TwimlLike f Reject a
reject a = iliftF . inj $ RejectF a
