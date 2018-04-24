{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Gather
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
-- <https://www.twilio.com/docs/api/twiml/gather TwiML Reference for \<Gather\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Gather
  ( gather
  , Gather
  , GatherF
  , GatherAttributes
  ) where

import Data.Void
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
        gather (def & action .~ parseURL "/process_gather.php"
                    & method .~ Just GET) $ do
          say "Please enter your account number, followed by the pound sign" def
          end
        say "We didn't receive any input. Goodbye!" def
        end
      where Twiml.Syntax{..} = def
:}

>>> putStr $ show example
<?xml version="1.0" encoding="UTF-8"?>
<Response>
  <Gather action="/process_gather.php" method="GET">
    <Say>Please enter your account number, followed by the pound sign</Say>
  </Gather>
  <Say>We didn&#39;t receive any input. Goodbye!</Say>
</Response>
-}
gather :: (IsTwimlLike f Gather, Nest i In Gather) => GatherAttributes -> TwimlLike' VoiceVerbsF i Void -> TwimlLike f Gather ()
gather a b = iliftF . inj $ GatherF a b ()
