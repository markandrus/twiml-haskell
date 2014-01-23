{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeOperators #-}

module Text.XML.Twiml.Verbs.Record
  ( -- * @\<Record\>@
    -- $record
    Record
    -- ** Constructors
  , record
  , record'
    -- ** Attributes
  , RecordAttributes(..)
  , defaultRecordAttributes
    -- *** Lenses
  , recordAttributes
  , maxLength
  , transcribe
  , transcribeCallback
  , playBeep
  , action
  , method
  , timeout
  , finishOnKey
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

{- $record This example

@
module Example where

import Control.Lens
import Text.XML.Twiml

example
  = respond
  . (record \<&\> timeout    .~ 10
            \<&\> transcribe .~ True)
  $ end
@

produces the following TwiML response:

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response\>
  \<Record timeout=\"10\" transcribe=\"true\" \/\>
\<\/Response\>
@
-}

newtype Record p = Record { fromRecord :: Twiml' p }
instance (p :/~ Gather') => Twiml p (Record p) where toTwiml' = fromRecord

record :: (Twiml p t, p :/~ Gather') => t -> Record p
record = record' defaultRecordAttributes

record' :: (Twiml p t, p :/~ Gather')
        => RecordAttributes -> t -> Record p
record' attrs
  = Record . Fix . RecordF attrs . toTwiml'

recordAttributes :: Lens' (Record p) RecordAttributes
recordAttributes = lens
  (\(Record (Fix (RecordF attributes _))) -> attributes)
  (\(Record (Fix (RecordF _          a)))    attributes ->
     Record (Fix (RecordF attributes a)))

setRecordAction :: RecordAttributes -> URL -> RecordAttributes
setRecordAction attrs action = attrs { recordAction = Just action }

setRecordMethod :: RecordAttributes -> Method -> RecordAttributes
setRecordMethod attrs method = attrs { recordMethod = Just method }

setRecordTimeout :: RecordAttributes -> Natural -> RecordAttributes
setRecordTimeout attrs timeout = attrs { recordTimeout = Just timeout }

setRecordFinishOnKey :: RecordAttributes -> Key -> RecordAttributes
setRecordFinishOnKey attrs key = attrs { recordFinishOnKey = Just key }

setRecordMaxLength :: RecordAttributes -> Natural -> RecordAttributes
setRecordMaxLength attrs length = attrs { recordMaxLength = Just length }

setRecordTranscribe :: RecordAttributes -> Bool -> RecordAttributes
setRecordTranscribe attrs transcribe
  = attrs { recordTranscribe = Just transcribe }

setRecordTranscribeCallback :: RecordAttributes -> URL -> RecordAttributes
setRecordTranscribeCallback attrs transcribeCallback
  = attrs { recordTranscribeCallback = Just transcribeCallback }

setRecordPlayBeep :: RecordAttributes -> Bool -> RecordAttributes
setRecordPlayBeep attrs playBeep = attrs { recordPlayBeep = Just playBeep }

maxLength :: Lens (Record p) (Record p) (Maybe Natural) Natural
maxLength = lens (^. recordAttributes . to' recordMaxLength)
  (\t v -> over recordAttributes (flip setRecordMaxLength v) t)

transcribe :: Lens (Record p) (Record p) (Maybe Bool) Bool
transcribe = lens (^. recordAttributes . to' recordTranscribe)
  (\t v -> over recordAttributes (flip setRecordTranscribe v) t)

transcribeCallback :: Lens (Record p) (Record p) (Maybe URL) URL
transcribeCallback = lens (^. recordAttributes . to' recordTranscribeCallback)
  (\t v -> over recordAttributes (flip setRecordTranscribeCallback v) t)

playBeep :: Lens (Record p) (Record p) (Maybe Bool) Bool
playBeep = lens (^. recordAttributes . to' recordPlayBeep)
  (\t v -> over recordAttributes (flip setRecordPlayBeep v) t)

instance HasAction (Record p) where
  action = lens getAction setAction where
    getAction = (^. recordAttributes . to' recordAction)
    setAction t v = over recordAttributes (flip setRecordAction v) t

instance HasMethod (Record p) where
  method = lens getMethod setMethod where
    getMethod = (^. recordAttributes . to' recordMethod)
    setMethod t v = over recordAttributes (flip setRecordMethod v) t

instance HasTimeout (Record p) where
  timeout = lens getTimeout setTimeout where
    getTimeout = (^. recordAttributes . to' recordTimeout)
    setTimeout t v = over recordAttributes (flip setRecordTimeout v) t

instance HasFinishOnKey (Record p) where
  finishOnKey = lens getFinishOnKey setFinishOnKey where
    getFinishOnKey = (^. recordAttributes . to' recordFinishOnKey)
    setFinishOnKey t v = over recordAttributes (flip setRecordFinishOnKey v) t
