{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Pause
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
-- <https://www.twilio.com/docs/api/twiml/pause TwiML Reference for \<Pause\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Pause
  ( pause
    -- * Data Types
  , Pause
  , PauseF(..)
    -- ** Attributes
  , PauseAttributes
    -- * Attribute Lenses
  , HasDuration(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
import Text.XML.Twiml.Lenses

{- | Example:

#include "pauseExample1.txt"
-}
pause :: IsTwimlLike f Pause => PauseAttributes -> TwimlLike f Pause ()
pause a = iliftF . inj $ PauseF a ()
