{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Play
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

{- | Example:

#include "playExample1.txt"
-}
play :: IsTwimlLike f Play => URL -> PlayAttributes -> TwimlLike f Play ()
play a b = iliftF . inj $ PlayF (pure a) b ()

{- | Example:

#include "playExample2.txt"
-}
play' :: IsTwimlLike f Play => Maybe URL -> PlayAttributes -> TwimlLike f Play ()
play' a b = iliftF . inj $ PlayF a b ()
