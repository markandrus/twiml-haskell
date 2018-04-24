{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Redirect
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
-- import Data.Maybe
-- import Text.XML.Twiml
-- import qualified Text.XML.Twiml.Syntax as Twiml
-- @
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/redirect TwiML Reference for \<Redirect\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Redirect
  ( redirect
  , Redirect
  , RedirectF
  , RedirectAttributes
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Types

-- $setup
-- >>> :set -XRebindableSyntax
-- >>> :set -XRecordWildCards
-- >>> import Prelude
-- >>> import Data.Default
-- >>> import Data.Maybe
-- >>> import Text.XML.Twiml
-- >>> import qualified Text.XML.Twiml.Syntax as Twiml

{- | Example:

>>> :{
let example :: VoiceTwiml
    example =
      voiceResponse $ do
        dial "415-123-4567" def
        redirect (fromJust $ parseURL "http://www.foo.com/nextInstructions") def
        end
      where Twiml.Syntax{..} = def
:}

>>> putStr $ show example
<?xml version="1.0" encoding="UTF-8"?>
<Response>
  <Dial>415-123-4567</Dial>
  <Redirect>http://www.foo.com/nextInstructions</Redirect>
</Response>
-}
redirect :: IsTwimlLike f Redirect => URL -> RedirectAttributes -> TwimlLike f Redirect a
redirect a b = iliftF . inj $ RedirectF a b
