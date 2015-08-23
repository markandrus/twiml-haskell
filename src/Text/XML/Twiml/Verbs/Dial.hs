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
  , Dial
  , DialF
  , DialAttributes
    -- * Nouns
  , dialNoun
  , DialNoun
  , DialNounF
    -- ** Client
  , client
  , Client
  , ClientF
  , ClientAttributes
    -- ** Conference
  , conference
  , Conference
  , ConferenceF
  , ConferenceAttributes
    -- ** Number
  , number
  , Number
  , NumberF
  , NumberAttributes
    -- ** Queue
  , queue
  , Queue
  , QueueF
  , QueueAttributes
    -- ** Sip
  , sip
  , Sip
  , SipF
  , SipAttributes
  ) where

import Data.Void
import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml
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

dialNoun :: TwimlLike DialNounF i Void -> DialNoun
dialNoun = DialNoun

client :: IsTwimlLike f Client => String -> ClientAttributes -> TwimlLike f Client a
client a b = iliftF . inj $ ClientF a b

conference :: IsTwimlLike f Conference => String -> ConferenceAttributes -> TwimlLike f Conference a
conference a b = iliftF . inj $ ConferenceF a b

number :: IsTwimlLike f Number => String -> NumberAttributes -> TwimlLike f Number a
number a b = iliftF . inj $ NumberF a b

queue :: IsTwimlLike f Queue => String -> QueueAttributes -> TwimlLike f Queue a
queue a b = iliftF . inj $ QueueF a b

sip :: IsTwimlLike f Sip => URL -> SipAttributes -> TwimlLike f Sip a
sip a b = iliftF . inj $ SipF a b
