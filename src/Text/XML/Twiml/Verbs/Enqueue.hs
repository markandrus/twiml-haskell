{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Enqueue
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
-- <https://www.twilio.com/docs/api/twiml/enqueue TwiML Reference for \<Enqueue\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Enqueue
  ( enqueue
  , Enqueue
  , EnqueueF
  , EnqueueAttributes
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

{- | Enqueue a caller in a queue. Example:

>>> :{
let example :: VoiceTwiml
    example =
      voiceResponse $ do
        enqueue "support" $ def & waitURL .~ parseURL "wait-music.xml"
        end
      where Twiml.Syntax{..} = def
:}

>>> putStr $ show example
<?xml version="1.0" encoding="UTF-8"?>
<Response>
  <Enqueue waitUrl="wait-music.xml">support</Enqueue>
</Response>
-}
enqueue :: IsTwimlLike f Enqueue => String -> EnqueueAttributes -> TwimlLike f Enqueue ()
enqueue a b = iliftF . inj $ EnqueueF a b ()
