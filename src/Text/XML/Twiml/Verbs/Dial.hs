{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Dial
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- The examples in this file assume
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
-- <https://www.twilio.com/docs/api/twiml/dial TwiML Reference for \<Dial\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Dial
  ( dial
  , dial'
    -- * Data Types
  , Dial
  , DialF(..)
    -- ** Attributes
  , DialAttributes
    -- * Attribute Lenses
  , HasAction(..)
  , HasMethod(..)
  , HasTimeout(..)
  , HasCallerId(..)
  , HasHangupOnStar(..)
  , HasRecord'(..)
  , HasTimeLimit(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses
import Text.XML.Twiml.Types

{- | Dial a number. Example:

#include "dialExample1.txt"
-}
dial :: IsTwimlLike f Dial => String -> DialAttributes -> TwimlLike f Dial ()
dial a b = iliftF . inj $ DialF (pure a) b ()

{- | Dial a number or 'DialNoun'. Example:

#include "dialExample3.txt"
-}
dial' :: IsTwimlLike f Dial => Either DialNoun String -> DialAttributes -> TwimlLike f Dial ()
dial' a b = iliftF . inj $ DialF a b ()
