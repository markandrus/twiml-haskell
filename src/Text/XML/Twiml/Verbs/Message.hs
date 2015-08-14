{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Message
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
-- <https://www.twilio.com/docs/api/twiml/sms/message TwiML Reference for \<Message\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Message
  ( message
    -- * Data Types
  , Message
  , MessageF(..)
    -- ** Attributes
  , MessageAttributes
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

@
example :: MessagingTwiml
example =
  response $ do
    'end'
  where Twiml.Syntax{..} = def
@

> <?xml version="1.0" encoding="UTF-8"?>
> <Response>
>  <Message>Store Location: 123 Easy St.</Message>
> </Response>
-}
message :: IsTwimlLike f Message => String -> MessageAttributes -> TwimlLike f Message ()
message a b = iliftF . inj $ MessageF a b ()
