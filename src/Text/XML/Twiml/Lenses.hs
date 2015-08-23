{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Lenses
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- This module defines lenses for use with TwiML attributes. Lenses are
-- re-exported by the @<Text-XML-Twiml-Verbs.html Text.XML.Twiml.Verbs> modules.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Lenses where

import Control.Lens hiding (to)
import Text.XML.Twiml.Types hiding (ClientAttributes(..), ConferenceAttributes(..), NumberAttributes(..), QueueAttributes(..), SipAttributes(..), DialNoun(..))
import Text.XML.Twiml.Internal.Twiml

makeLensesWith abbreviatedFields ''SayAttributes
makeLensesWith abbreviatedFields ''PlayAttributes
makeLensesWith abbreviatedFields ''GatherAttributes
makeLensesWith abbreviatedFields ''RecordAttributes
makeLensesWith abbreviatedFields ''SmsAttributes
makeLensesWith abbreviatedFields ''DialAttributes
makeLensesWith abbreviatedFields ''DialNoun
makeLensesWith abbreviatedFields ''NumberAttributes
makeLensesWith abbreviatedFields ''SipAttributes
makeLensesWith abbreviatedFields ''ClientAttributes
makeLensesWith abbreviatedFields ''ConferenceAttributes
makeLensesWith abbreviatedFields ''QueueAttributes
makeLensesWith abbreviatedFields ''EnqueueAttributes
makeLensesWith abbreviatedFields ''RedirectAttributes
makeLensesWith abbreviatedFields ''RejectAttributes
makeLensesWith abbreviatedFields ''PauseAttributes
makeLensesWith abbreviatedFields ''MessageAttributes
