{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Play
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
-- import Data.Maybe
-- import Text.XML.Twiml
-- import qualified Text.XML.Twiml.Syntax as Twiml
-- @
--
-- For more information, refer to Twilio's
-- <https://www.twilio.com/docs/api/twiml/play TwiML Reference for \<Play\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Play
  ( play
  , play'
  , Play
  , PlayF
  , PlayAttributes
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Types

-- $setup
-- >>> :set -XRebindableSyntax
-- >>> :set -XRecordWildCards
-- >>> import Prelude
-- >>> import Control.Lens
-- >>> import Data.Default
-- >>> import Data.Maybe
-- >>> import Text.XML.Twiml
-- >>> import qualified Text.XML.Twiml.Syntax as Twiml

{- | Example:

>>> :{
let example1 :: VoiceTwiml
    example1 =
      voiceResponse $ do
        play (fromJust $ parseURL "https://api.twilio.com/cowbell.mp3") def
        end
      where Twiml.Syntax{..} = def
:}

>>> putStr $ show example1
<?xml version="1.0" encoding="UTF-8"?>
<Response>
  <Play>https://api.twilio.com/cowbell.mp3</Play>
</Response>
-}
play :: IsTwimlLike f Play => URL -> PlayAttributes -> TwimlLike f Play ()
play a b = iliftF . inj $ PlayF (pure a) b ()

{- | Example:

>>> :{
let example2 :: VoiceTwiml
    example2 =
      voiceResponse $ do
        play' Nothing $ def & digits .~ Just [W, W, W, W, D3]
        end
      where Twiml.Syntax{..} = def
:}

>>> putStr $ show example2
<?xml version="1.0" encoding="UTF-8"?>
<Response>
  <Play digits="wwww3" />
</Response>
-}
play' :: IsTwimlLike f Play => Maybe URL -> PlayAttributes -> TwimlLike f Play ()
play' a b = iliftF . inj $ PlayF a b ()
