{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Dial where

import Text.XML.Twiml.Internal (Fix(..), Lang(..), LangAlice(..), DialAttributes(..), setDialHangupOnStar, setDialTimeLimit, setDialCallerId, setDialRecord, defaultDialAttributes, Twiml(..), Twiml', TwimlF(..), URL(..), NotGatherNoun, DialNoun(..), Natural)
import Text.XML.Twiml.Internal.Lens ((^.), Lens, Lens', lens, over, to')

newtype Dial p = Dial { fromDial :: Twiml' p }
instance NotGatherNoun p => Twiml p (Dial p) where toTwiml' = fromDial

dial :: (Twiml p t, NotGatherNoun p) => Either DialNoun String -> t -> Dial p
dial = dial' defaultDialAttributes

dial' :: (Twiml p t, NotGatherNoun p)
      => DialAttributes -> Either DialNoun String -> t -> Dial p
dial' attrs n = Dial . Fix . DialF attrs n . toTwiml'

dialAttributes :: Lens' (Dial p) DialAttributes
dialAttributes = lens
  (\(Dial (Fix (DialF attributes _ _))) -> attributes)
  (\(Dial (Fix (DialF _          n a)))    attributes ->
     Dial (Fix (DialF attributes n a)))

hangupOnStar :: Lens (Dial p) (Dial p) (Maybe Bool) Bool
hangupOnStar = lens (^. dialAttributes . to' dialHangupOnStar)
  (\t v -> over dialAttributes (flip setDialHangupOnStar v) t)

timeLimit :: Lens (Dial p) (Dial p) (Maybe Natural) Natural
timeLimit = lens (^. dialAttributes . to' dialTimeLimit)
  (\t v -> over dialAttributes (flip setDialTimeLimit v) t)

callerId :: Lens (Dial p) (Dial p) (Maybe String) String
callerId = lens (^. dialAttributes . to' dialCallerId)
  (\t v -> over dialAttributes (flip setDialCallerId v) t)

recordDial :: Lens (Dial p) (Dial p) (Maybe Bool) Bool
recordDial = lens (^. dialAttributes . to' dialRecord)
  (\t v -> over dialAttributes (flip setDialRecord v) t)
