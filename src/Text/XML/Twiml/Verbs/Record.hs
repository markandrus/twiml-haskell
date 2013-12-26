{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Record where

import Text.XML.Twiml.Internal (Fix(..), Lang(..), LangAlice(..), RecordAttributes(..), setRecordTranscribe, setRecordTranscribeCallback, setRecordMaxLength, setRecordPlayBeep, defaultRecordAttributes, Twiml(..), Twiml', TwimlF(..), URL(..), NotGatherNoun, Natural)
import Text.XML.Twiml.Internal.Lens ((^.), Lens, Lens', lens, over, to')

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

