{-# OPTIONS_GHC -cpp -optP -P -Itest/examples #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.Enqueue
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

{- | Enqueue a caller in a queue. Example:

#include "enqueueExample1.txt"
-}
enqueue :: IsTwimlLike f Enqueue => String -> EnqueueAttributes -> TwimlLike f Enqueue ()
enqueue a b = iliftF . inj $ EnqueueF a b ()
