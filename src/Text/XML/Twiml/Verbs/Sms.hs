{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Sms
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
-- <https://www.twilio.com/docs/api/twiml/sms TwiML Reference for \<Sms\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Sms
  ( sms
    -- * Data Types
  , Sms
  , SmsF(..)
    -- ** Attributes
  , SmsAttributes
    -- * Attribute Lenses
  , HasAction(..)
  , HasMethod(..)
  , HasFrom(..)
  , HasStatusCallback(..)
  , HasTo(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses

{- | Example:

#include "smsExample2.txt"
-}
sms :: IsTwimlLike f Sms => String -> SmsAttributes -> TwimlLike f Sms ()
sms a b = iliftF . inj $ SmsF a b ()
