{-#LANGUAGE DataKinds #-}
{-#LANGUAGE DeriveAnyClass #-}
{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE EmptyDataDecls #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverlappingInstances #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE UndecidableInstances #-}

module Text.XML.Twiml.Internal
  ( (:+:)(..)
  , (:<:)(..)
  , response
  , Voice'
  , Messaging
  , VoiceTwimlF(..)
  , MessagingTwimlF(..)
  , SayF'(..)
  , say
  , say'
  , PlayF'(..)
  , play
  , play'
  , play''
  , play'''
  , GatherF'(..)
  , gather
  , gather'
  , RecordF'(..)
  , record
  , record''
  , SmsF'(..)
  , sms
  , sms'
  , DialF'(..)
  , dial
  , dial'
  , dial''
  , dial'''
  , EnqueueF'(..)
  , enqueue
  , enqueue'
  , LeaveF'(..)
  , leave
  , leave'
  , HangupF'(..)
  , hangup
  , hangup'
  , RedirectF'(..)
  , redirect
  , redirect'
  , RejectF'(..)
  , reject
  , reject'
  , PauseF'(..)
  , pause
  , pause'
  , EndF'(..)
  , end
  , end'
  , Twiml(..)
  , Twiml'
  , In
  , Nest
  , TwimlF(..)
  , Say
  , Play
  , Gather
  , Record
  , Sms
  , Dial
  , Enqueue
  , Leave
  , Hangup
  , Redirect
  , Reject
  , Pause
  , End
  ) where

import Text.XML.Twiml.Types hiding (say, play, play', gather, record, sms, dial,
  dial', enqueue, leave, hangup, redirect, reject, pause, end, Twiml, response,
  showTwiml)
import Text.XML.Light

import Control.DeepSeq (NFData(..))
import Data.Data
import Data.Void
import GHC.Generics (Generic)

response :: Twiml' i Void -> Twiml
response = Response

say :: String -> SayAttributes -> Twiml' '[Say] ()
say a b = iliftF $ SayF a b ()

say' :: String -> SayAttributes -> VoiceTwiml' '[Say] ()
say' a b = iliftF . inj $ SayF' a b ()

play :: URL -> PlayAttributes -> Twiml' '[Play] ()
play a b = iliftF $ PlayF (Just a) b ()

play' :: Maybe URL -> PlayAttributes -> Twiml' '[Play] ()
play' a b = iliftF $ PlayF a b ()

play'' :: URL -> PlayAttributes -> VoiceTwiml' '[Play] ()
play'' a b = iliftF . inj $ PlayF' (Just a) b ()

play''' :: Maybe URL -> PlayAttributes -> VoiceTwiml' '[Play] ()
play''' a b = iliftF . inj $ PlayF' a b ()

gather :: Nest i In Gather
       => GatherAttributes -> Twiml' i Void -> Twiml' '[Gather] ()
gather a b = iliftF $ GatherF a b ()

gather' :: Nest i In Gather
        => GatherAttributes -> VoiceTwiml' i Void -> VoiceTwiml' '[Gather] ()
gather' a b = iliftF . inj $ GatherF' a b ()

record :: RecordAttributes -> Twiml' '[Record] ()
record a = iliftF $ RecordF a ()

record'' :: RecordAttributes -> VoiceTwiml' '[Record] ()
record'' a = iliftF . inj $ RecordF' a ()

sms :: String -> SmsAttributes -> Twiml' '[Sms] ()
sms a b = iliftF $ SmsF a b ()

sms' :: String -> SmsAttributes -> VoiceTwiml' '[Sms] ()
sms' a b = iliftF . inj $ SmsF' a b ()

dial :: String -> DialAttributes -> Twiml' '[Dial] ()
dial a b = iliftF $ DialF (Right a) b ()

dial' :: Either DialNoun String -> DialAttributes -> Twiml' '[Dial] ()
dial' a b = iliftF $ DialF a b ()

dial'' :: String -> DialAttributes -> VoiceTwiml' '[Dial] ()
dial'' a b = iliftF . inj $ DialF' (Right a) b ()

dial''' :: Either DialNoun String -> DialAttributes -> VoiceTwiml' '[Dial] ()
dial''' a b = iliftF . inj $ DialF' a b ()

enqueue :: String -> EnqueueAttributes -> Twiml' '[Enqueue] ()
enqueue a b = iliftF $ EnqueueF a b ()

enqueue' :: String -> EnqueueAttributes -> VoiceTwiml' '[Enqueue] ()
enqueue' a b = iliftF . inj $ EnqueueF' a b ()

leave :: Twiml' '[Leave] a
leave = iliftF LeaveF

leave' :: VoiceTwiml' '[Leave] a
leave' = iliftF $ inj LeaveF'

hangup :: Twiml' '[Hangup] a
hangup = iliftF HangupF

hangup' :: VoiceTwiml' '[Hangup] a
hangup' = iliftF $ inj HangupF'

redirect :: URL -> RedirectAttributes -> Twiml' '[Redirect] a
redirect a b = iliftF $ RedirectF a b

redirect' :: URL -> RedirectAttributes -> VoiceTwiml' '[Redirect] a
redirect' a b = iliftF . inj $ RedirectF' a b

reject :: RejectAttributes -> Twiml' '[Reject] a
reject a = iliftF $ RejectF a

reject' :: RejectAttributes -> VoiceTwiml' '[Reject] a
reject' a = iliftF . inj $ RejectF' a

pause :: PauseAttributes -> Twiml' '[Pause] ()
pause a = iliftF $ PauseF a ()

pause' :: PauseAttributes -> VoiceTwiml' '[Pause] ()
pause' a = iliftF . inj $ PauseF' a ()

end :: Twiml' '[] a
end = iliftF EndF

end' :: VoiceTwiml' '[End] Void
end' = iliftF $ inj EndF'

data Twiml = forall i. Response (Twiml' (i :: [*]) Void)

instance NFData Twiml where
  rnf (Response twiml) = rnf twiml

instance Show Twiml where
  show = showTwiml -- show (Response a) = "Response (" ++ show a ++ ")"

type Twiml' = IxFree TwimlF

data TwimlF i a where
  SayF      :: String
            -> SayAttributes
            -> a
            -> TwimlF '[Say] a
  PlayF     :: Maybe URL
            -> PlayAttributes
            -> a
            -> TwimlF '[Play] a
  GatherF   :: Nest i In Gather
            => GatherAttributes
            -> Twiml' i Void
            -> a
            -> TwimlF '[Gather] a
  RecordF   :: RecordAttributes
            -> a
            -> TwimlF '[Record] a
  SmsF      :: String
            -> SmsAttributes
            -> a
            -> TwimlF '[Sms] a
  DialF     :: Either DialNoun String
            -> DialAttributes
            -> a
            -> TwimlF '[Dial] a
  EnqueueF  :: String
            -> EnqueueAttributes
            -> a
            -> TwimlF '[Enqueue] a
  LeaveF    :: TwimlF '[Leave] a
  HangupF   :: TwimlF '[Hangup] a
  RedirectF :: URL
            -> RedirectAttributes
            -> TwimlF '[Redirect] a
  RejectF   :: RejectAttributes
            -> TwimlF '[Reject] a
  PauseF    :: PauseAttributes
            -> a
            -> TwimlF '[Pause] a
  EndF      :: TwimlF '[] a

newtype MessagingTwimlF i a = MessagingTwimlF
  { getMessagingTwimlF ::
    (     SmsF'      i
      :+: RedirectF' i
    ) a
  } deriving (Eq, Functor, Generic, Show)

instance IxShow MessagingTwimlF where
  ishow = show

type MessagingTwiml' i a = IxFree MessagingTwimlF i a

instance Functor (TwimlF i) where
  fmap f (SayF      a b c) = SayF      a b $ f c
  fmap f (PlayF     a b c) = PlayF     a b $ f c
  fmap f (GatherF   a b c) = GatherF   a b $ f c
  fmap f (RecordF   a b)   = RecordF   a   $ f b
  fmap f (SmsF      a b c) = SmsF      a b $ f c
  fmap f (DialF     a b c) = DialF     a b $ f c
  fmap f (EnqueueF  a b c) = EnqueueF  a b $ f c
  fmap _  LeaveF           = LeaveF
  fmap _  HangupF          = HangupF
  fmap _ (RedirectF a b)   = RedirectF a b
  fmap _ (RejectF   a)     = RejectF   a
  fmap f (PauseF    a b)   = PauseF    a   $ f b
  fmap _  EndF             = EndF

instance IxFunctor TwimlF where
  imap = fmap

instance Show a => Show (TwimlF i a) where
  show (SayF      a b c) = "SayF ("      ++ show a ++ ") (" ++ show b ++ ") (" ++ show c ++ ")"
  show (PlayF     a b c) = "PlayF ("     ++ show a ++ ") (" ++ show b ++ ") (" ++ show c ++ ")"
  show (GatherF   a b c) = "GatherF ("   ++ show a ++ ") (" ++ show b ++ ") (" ++ show c ++ ")"
  show (RecordF   a b)   = "RecordF ("   ++ show a ++ ") (" ++ show b ++ ")"
  show (SmsF      a b c) = "SmsF ("      ++ show a ++ ") (" ++ show b ++ ") (" ++ show c ++ ")"
  show (DialF     a b c) = "DialF ("     ++ show a ++ ") (" ++ show b ++ ") (" ++ show c ++ ")"
  show (EnqueueF  a b c) = "EnqueueF ("  ++ show a ++ ") (" ++ show b ++ ") (" ++ show c ++ ")"
  show  LeaveF           = "LeaveF"
  show  HangupF          = "HangupF"
  show (RedirectF a b)   = "RedirectF (" ++ show a ++ ") (" ++ show b ++ ")"
  show (RejectF   a)     = "RejectF ("   ++ show a ++ ")"
  show (PauseF    a b)   = "PauseF ("    ++ show a ++ ") (" ++ show b
  show  EndF             = "EndF"

instance IxShow TwimlF where
  ishow = show

instance IxNFData TwimlF where
  irnf (SayF      a b c) = rnf a `seq` rnf b `seq` rnf c
  irnf (PlayF     a b c) = rnf a `seq` rnf b `seq` rnf c
  irnf (GatherF   a b c) = rnf a `seq` rnf b `seq` rnf c
  irnf (RecordF   a b)   = rnf a `seq` rnf b
  irnf (SmsF      a b c) = rnf a `seq` rnf b `seq` rnf c
  irnf (DialF     a b c) = rnf a `seq` rnf b `seq` rnf c
  irnf (EnqueueF  a b c) = rnf a `seq` rnf b `seq` rnf c
  irnf  LeaveF           = ()
  irnf  HangupF          = ()
  irnf (RedirectF a b)   = rnf a `seq` rnf b
  irnf (RejectF   a)     = rnf a
  irnf (PauseF    a b)   = rnf a `seq` rnf b
  irnf  EndF             = ()

instance NFData a => NFData (TwimlF i a) where
  rnf = irnf

showTwiml :: Twiml -> String
showTwiml twiml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ ppElement (toElement twiml) ++ "\n"

instance ToElement Twiml where
  toElement (Response twiml) = unode "Response" $ toXML twiml

instance ToXML (Twiml' i Void) where
  toXML (IxFree twiml) = toXML twiml
  toXML (IxPure _) = error "Impossible"

instance ToXML (TwimlF i (Twiml' j Void)) where
  toXML (SayF      a         attrs b) =  makeElement "Say"      (strToContent a)  (toAttrs attrs) : toXML b
  toXML (PlayF     (Just a)  attrs b) =  makeElement "Play"     (urlToContent a)  (toAttrs attrs) : toXML b
  toXML (PlayF     _         attrs b) =  makeElement "Play"     ()                (toAttrs attrs) : toXML b
  toXML (GatherF   attrs     a     b) =  makeElement "Gather"   (toXML        a)  (toAttrs attrs) : toXML b
  toXML (RecordF             attrs a) =  makeElement "Record"   ()                (toAttrs attrs) : toXML a
  toXML (SmsF      a         attrs b) =  makeElement "Sms"      (strToContent a)  (toAttrs attrs) : toXML b
  toXML (DialF     (Left  a) attrs b) =  makeElement "Dial"     (toElement    a)  (toAttrs attrs) : toXML b
  toXML (DialF     (Right a) attrs b) =  makeElement "Dial"     (strToContent a)  (toAttrs attrs) : toXML b
  toXML (EnqueueF  a         attrs b) =  makeElement "Enqueue"  (strToContent a)  (toAttrs attrs) : toXML b
  toXML  LeaveF                       = [makeElement "Leave"    ()               []]
  toXML  HangupF                      = [makeElement "Hangup"   ()               []]
  toXML (RedirectF a         attrs)   = [makeElement "Redirect" (urlToContent a) $ toAttrs attrs]
  toXML (RejectF             attrs)   = [makeElement "Reject"   ()               $ toAttrs attrs]
  toXML (PauseF              attrs a) =  makeElement "Pause"    ()                (toAttrs attrs) : toXML a
  toXML  EndF                         = []

-- instance ToXML (VoiceTwiml' i Void) where
