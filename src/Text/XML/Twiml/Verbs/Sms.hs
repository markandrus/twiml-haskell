{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Sms where

import Text.XML.Twiml.Internal (Fix(..), Lang(..), LangAlice(..), SmsAttributes(..), setSmsTo, setSmsFrom, setSmsStatusCallback, defaultSmsAttributes, Twiml(..), Twiml', TwimlF(..), URL(..), NotGatherNoun, Natural)
import Text.XML.Twiml.Internal.Lens ((^.), Lens, Lens', lens, over, to')

newtype Sms p = Sms { fromSms :: Twiml' p }
instance NotGatherNoun p => Twiml p (Sms p) where toTwiml' = fromSms

sms :: (Twiml p t, NotGatherNoun p) => String -> t -> Sms p
sms = sms' defaultSmsAttributes

sms' :: (Twiml p t, NotGatherNoun p) => SmsAttributes -> String -> t -> Sms p
sms' attrs n = Sms . Fix . SmsF attrs n . toTwiml'

smsAttributes :: Lens' (Sms p) SmsAttributes
smsAttributes = lens
  (\(Sms (Fix (SmsF attributes _ _))) -> attributes)
  (\(Sms (Fix (SmsF _          n a)))    attributes ->
     Sms (Fix (SmsF attributes n a)))

to :: Lens (Sms p) (Sms p) (Maybe String) String
to = lens (^. smsAttributes . to' smsTo)
  (\t v -> over smsAttributes (flip setSmsTo v) t)

from :: Lens (Sms p) (Sms p) (Maybe String) String
from = lens (^. smsAttributes . to' smsFrom)
  (\t v -> over smsAttributes (flip setSmsFrom v) t)

statusCallback :: Lens (Sms p) (Sms p) (Maybe URL) URL
statusCallback = lens (^. smsAttributes . to' smsStatusCallback)
  (\t v -> over smsAttributes (flip setSmsStatusCallback v) t)
