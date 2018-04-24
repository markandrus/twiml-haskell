{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Hangup
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
-- import Data.Default
-- import Text.XML.Twiml
-- import qualified Text.XML.Twiml.Syntax as Twiml
-- @
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/hangup TwiML Reference for \<Hangup\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Hangup
  ( hangup
  , Hangup
  , HangupF
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml

-- $setup
-- >>> :set -XRebindableSyntax
-- >>> :set -XRecordWildCards
-- >>> import Prelude
-- >>> import Data.Default
-- >>> import Text.XML.Twiml
-- >>> import qualified Text.XML.Twiml.Syntax as Twiml

{- | Hangup a call. Example:

>>> :{
let example :: VoiceTwiml
    example =
      voiceResponse $ do
        hangup
        end
      where Twiml.Syntax{..} = def
:}

>>> putStr $ show example
<?xml version="1.0" encoding="UTF-8"?>
<Response>
  <Hangup />
</Response>
-}
hangup :: IsTwimlLike f Hangup => TwimlLike f Hangup a
hangup = iliftF . inj $ HangupF
