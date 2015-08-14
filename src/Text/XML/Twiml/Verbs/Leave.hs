{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Leave
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
-- <https://www.twilio.com/docs/api/twiml/leave TwiML Reference for \<Leave\>>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.Leave
  ( leave
    -- * Data Types
  , Leave
  , LeaveF(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml

{- | Leave a queue. Example:

#include "leaveExample1.txt"
-}
leave :: IsTwimlLike f Leave => TwimlLike f Leave a
leave = iliftF . inj $ LeaveF
