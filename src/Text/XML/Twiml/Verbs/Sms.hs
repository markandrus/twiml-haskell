{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeOperators #-}

module Text.XML.Twiml.Verbs.Sms
  ( -- * @\<Sms\>@
    -- $sms
    Sms
    -- ** Constructors
  , sms
  , sms'
    -- ** Attributes
  , SmsAttributes(..)
  , defaultSmsAttributes
    -- *** Lenses
  , smsAttributes
  , to
  , from
  , statusCallback
  , action
  , method
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

{- $sms This example

@
module Example where

import Control.Lens
import Text.XML.Twiml

example
  = respond
  . (sms \"The king stay the king.\" \<&\> to   .~ \"+14105551234\"
                                   \<&\> from .~ \"+14105556789\")
  $ end
@

produces the following TwiML response:

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response\>
  \<Sms from=\"+14105551234\" to=\"+14105556789\"\>The king stay the king.\<\/Sms\>
\<\/Response\>
@
-}

newtype Sms p = Sms { fromSms :: Twiml' p }
instance (p :/~ Gather') => Twiml p (Sms p) where toTwiml' = fromSms

sms :: (Twiml p t, p :/~ Gather') => String -> t -> Sms p
sms = sms' defaultSmsAttributes

sms' :: (Twiml p t, p :/~ Gather') => SmsAttributes -> String -> t -> Sms p
sms' attrs n = Sms . Fix . SmsF attrs n . toTwiml'

smsAttributes :: Lens' (Sms p) SmsAttributes
smsAttributes = lens
  (\(Sms (Fix (SmsF attributes _ _))) -> attributes)
  (\(Sms (Fix (SmsF _          n a)))    attributes ->
     Sms (Fix (SmsF attributes n a)))

setSmsTo :: SmsAttributes -> String -> SmsAttributes
setSmsTo attrs to = attrs { smsTo = Just to }

setSmsFrom :: SmsAttributes -> String -> SmsAttributes
setSmsFrom attrs from = attrs { smsFrom = Just from }

setSmsAction :: SmsAttributes -> URL -> SmsAttributes
setSmsAction attrs action = attrs { smsAction = Just action }

setSmsMethod :: SmsAttributes -> Method -> SmsAttributes
setSmsMethod attrs method = attrs { smsMethod = Just method }

setSmsStatusCallback :: SmsAttributes -> URL -> SmsAttributes
setSmsStatusCallback attrs statusCallback
  = attrs { smsStatusCallback = Just statusCallback }

to :: Lens (Sms p) (Sms p) (Maybe String) String
to = lens (^. smsAttributes . to' smsTo)
  (\t v -> over smsAttributes (`setSmsTo` v) t)

from :: Lens (Sms p) (Sms p) (Maybe String) String
from = lens (^. smsAttributes . to' smsFrom)
  (\t v -> over smsAttributes (`setSmsFrom` v) t)

statusCallback :: Lens (Sms p) (Sms p) (Maybe URL) URL
statusCallback = lens (^. smsAttributes . to' smsStatusCallback)
  (\t v -> over smsAttributes (`setSmsStatusCallback` v) t)

instance HasAction (Sms p) where
  action = lens getAction setAction where
    getAction = (^. smsAttributes . to' smsAction)
    setAction t v = over smsAttributes (`setSmsAction` v) t

instance HasMethod (Sms p) where
  method = lens getMethod setMethod where
    getMethod = (^. smsAttributes . to' smsMethod)
    setMethod t v = over smsAttributes (`setSmsMethod` v) t
