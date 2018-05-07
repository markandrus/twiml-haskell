{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Pause
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
-- <https://www.twilio.com/docs/api/twiml/pause TwiML Reference for \<Pause\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Pause
  ( pause
  , Pause
  , PauseF
  , PauseAttributes
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
        say "I will pause 10 seconds starting now!" def
        pause $ def & duration .~ Just 10
        say "I just paused 10 seconds" def
        end
      where Twiml.Syntax{..} = def
:}

>>> putStr $ show example
<?xml version="1.0" encoding="UTF-8"?>
<Response>
  <Say>I will pause 10 seconds starting now!</Say>
  <Pause length="10" />
  <Say>I just paused 10 seconds</Say>
</Response>
-}
pause :: IsTwimlLike f Pause => PauseAttributes -> TwimlLike f Pause ()
pause a = iliftF . inj $ PauseF a ()
