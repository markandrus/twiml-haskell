{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Record
  ( -- * @\<Record\>@
    Record
  , record
  , record'
    -- * Attribute Lenses
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

newtype Record p = Record { fromRecord :: Twiml' p }
instance NotGatherNoun p => Twiml p (Record p) where toTwiml' = fromRecord

record :: (Twiml p t, NotGatherNoun p) => t -> Record p
record = record' defaultRecordAttributes

record' :: (Twiml p t, NotGatherNoun p)
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
