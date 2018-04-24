{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Sms
-- Copyright   :  (C) 2018 Mark Andrus Roberts
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
-- import Control.Lens
-- import Data.Default
-- import Text.XML.Twiml
-- import qualified Text.XML.Twiml.Syntax as Twiml
-- @
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/sms TwiML Reference for \<Sms\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Sms
  ( sms
  , Sms
  , SmsF
  , SmsAttributes
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml

-- $setup
-- >>> :set -XRebindableSyntax
-- >>> :set -XRecordWildCards
-- >>> import Prelude
-- >>> import Control.Lens
-- >>> import Data.Default
-- >>> import Text.XML.Twiml
-- >>> import qualified Text.XML.Twiml.Syntax as Twiml

{- | Example:

>>> :{
let example :: VoiceTwiml
    example =
      voiceResponse $ do
        say "Our store is located at 123 Easy St." def
        sms "Store Location: 123 Easy St." $ def
                & action .~ parseURL "/smsHandler.php"
                & method .~ Just POST
        end
      where Twiml.Syntax{..} = def
:}

>>> putStr $ show example
<?xml version="1.0" encoding="UTF-8"?>
<Response>
  <Say>Our store is located at 123 Easy St.</Say>
  <Sms action="/smsHandler.php" method="POST">Store Location: 123 Easy St.</Sms>
</Response>
-}
sms :: IsTwimlLike f Sms => String -> SmsAttributes -> TwimlLike f Sms ()
sms a b = iliftF . inj $ SmsF a b ()
