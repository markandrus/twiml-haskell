{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Redirect
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

{- | Example:

#include "redirectExample1.txt"
-}
redirect :: IsTwimlLike f Redirect => URL -> RedirectAttributes -> TwimlLike f Redirect a
redirect a b = iliftF . inj $ RedirectF a b
