{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.End
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
-- End is not actually a TwiML verb; it is used for terminating TwiML.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.End
  ( end
  , End
  , EndF
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

{- | Terminate a TwiML response, or construct an empty TwiML response. Example:

>>> :{
let example :: VoiceTwiml
    example =
      voiceResponse $ do
        end
      where Twiml.Syntax{..} = def
:}

>>> putStr $ show example
<?xml version="1.0" encoding="UTF-8"?>
<Response />
-}
end :: IsTwimlLike f End => TwimlLike f End a
end = iliftF $ inj EndF
