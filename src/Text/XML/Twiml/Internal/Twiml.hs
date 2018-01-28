{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Internal.Twiml
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- This module defines all of the TwiML verbs and nouns in a single place
-- (mainly due to a few mutually-recursive definitions). This modules also
-- exposeses the internals of each TwiML's attributes.
--
-- Prefer the definitions re-exported by the
-- <Text-XML-Twiml.html Text.XML.Twiml> and
-- <Text-XML-Twiml-Verbs.html Text.XML.Twiml.Verbs> modules to those exported
-- here.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Internal.Twiml
  ( -- * TwiML
    MessagingTwiml(..)
  , MessagingVerbsF(..)
  , VoiceTwiml(..)
  , VoiceVerbsF(..)
  , Base
  , IsTwimlLike
  , TwimlLike
  , TwimlLike'
  , response
  , responseMsg
    -- ** Nouns
  , DialNoun(..)
  , DialNounF(..)
    -- *** Client
  , Client
  , ClientF(..)
  , ClientAttributes(..)
    -- *** Conference
  , Conference
  , ConferenceF(..)
  , ConferenceAttributes(..)
    -- *** Number
  , Number
  , NumberF(..)
  , NumberAttributes(..)
    -- *** Queue
  , Queue
  , QueueF(..)
  , QueueAttributes(..)
    -- *** Sip
  , Sip
  , SipF(..)
  , SipAttributes(..)
    -- ** Verbs
    -- *** Dial
  , Dial
  , DialF(..)
  , DialAttributes(..)
    -- *** End
  , End
  , EndF(..)
    -- *** Enqueue
  , Enqueue
  , EnqueueF(..)
  , EnqueueAttributes(..)
    -- *** Hangup
  , Gather
  , GatherF(..)
  , GatherAttributes(..)
  , Nest
  , In
    -- *** Hangup
  , Hangup
  , HangupF(..)
    -- *** Leave
  , Leave
  , LeaveF(..)
    -- *** Message
  , Message
  , MessageF(..)
  , MessageAttributes(..)
    -- *** Pause
  , Pause
  , PauseF(..)
  , PauseAttributes(..)
    -- *** Play
  , Play
  , PlayF(..)
  , PlayAttributes(..)
    -- *** Record
  , Record
  , RecordF(..)
  , RecordAttributes(..)
    -- *** Redirect
  , Redirect
  , RedirectF(..)
  , RedirectAttributes(..)
    -- *** Reject
  , Reject
  , RejectF(..)
  , RejectAttributes(..)
    -- *** Say
  , Say
  , SayF(..)
  , SayAttributes(..)
    -- *** Sms
  , Sms
  , SmsF(..)
  , SmsAttributes(..)
  ) where

import Control.Monad
import Control.DeepSeq
import Data.Data
import Data.Default
import Data.Void
import GHC.Generics (Generic)
import Text.XML.Light
import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.TH
import Text.XML.Twiml.Types

-------------------------------------------------------------------------------
-- Nouns
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Client
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Client
  required
    String
  attributes
    url, URL
    method, Method
  toXMLForGADT
  toAttrsForAttributes
|]

-------------------------------------------------------------------------------
-- Conference
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Conference
  required
    String
  attributes
    muted, Bool
    beep, Bool
    startOnEnter, Bool, startConferenceOnEnter
    endOnExit, Bool, endConferenceOnExit
    waitURL, URL
    waitMethod, Method
    maxParticipants, Natural
  toXMLForGADT
  toAttrsForAttributes
|]

-------------------------------------------------------------------------------
-- Number
-------------------------------------------------------------------------------

type Digits = [Digit]

twimlSpecStringToData [s|
Number
  required
    String
  attributes
    sendDigits, Digits
    url, URL
    method, Method
  toXMLForGADT
  toAttrsForAttributes
|]

-------------------------------------------------------------------------------
-- Queue
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Queue
  required
    String
  attributes
    url, URL
    method, Method
  toXMLForGADT
  toAttrsForAttributes
|]

-------------------------------------------------------------------------------
-- Sip
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Sip
  required
    URL
  attributes
    username, String
    password, String
    transport, Transport
    headers, String
    url, URL
    method, Method
  toXMLForGADT
  toAttrsForAttributes
|]

data DialNoun = forall i. DialNoun (IxFree DialNounF i Void)

instance ToSomeNode DialNoun where
  toSomeNode (DialNoun dialNoun) = SomeNode $ toXML dialNoun

instance ToSomeNode EitherDialNounString where
  toSomeNode = either toSomeNode toSomeNode

instance ToXML DialNoun where
  toXML (DialNoun dialNoun) = toXML dialNoun

instance Show DialNoun where
  show = showDialNoun

showDialNoun :: DialNoun -> String
showDialNoun = concatMap ppElement . toXML

newtype DialNounF i a = DialNounF
  { getDialNounF ::
    ( ClientF     i :+:
      ConferenceF i :+:
      NumberF     i :+:
      QueueF      i :+:
      SipF        i ) a
  } deriving (Functor, Generic, Show, Typeable)

instance
  ( Functor (f i)
  , f i :<: ( ClientF     i :+:
              ConferenceF i :+:
              NumberF     i :+:
              QueueF      i :+:
              SipF        i )
  ) => f i :<: DialNounF i where
  inj = DialNounF . inj
  prj = prj . getDialNounF

instance Functor1 DialNounF where
  fmap1 = fmap

instance Show1 DialNounF where
  show1 = show

instance ToXML (DialNounF i a) where
  toXML = toXML . getDialNounF

instance ToXML (IxFree DialNounF i Void) where
  toXML (IxFree f) = toXML f
  toXML _ = error "Impossible"

-------------------------------------------------------------------------------
-- Verbs
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Dial
-------------------------------------------------------------------------------

type EitherDialNounString = Either DialNoun String

twimlSpecStringToData [s|
Dial
  required
    EitherDialNounString
  attributes
    action, URL
    method, Method
    timeout, Natural
    hangupOnStar, Bool
    timeLimit, Natural
    callerId, String
    record', Bool, record
  recursive
  toXMLForGADT
  toAttrsForAttributes
|]

-------------------------------------------------------------------------------
-- End
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
End
|]

instance ToXML (EndF i a) where
  toXML EndF = []

-------------------------------------------------------------------------------
-- Enqueue
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Enqueue
  required
    String
  attributes
    action, URL
    method, Method
    waitURL, URL, waitUrl
    waitMethod, Method, waitUrlMethod
  recursive
  toXMLForGADT
  toAttrsForAttributes
|]

-------------------------------------------------------------------------------
-- Hangup
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Hangup
  toXMLForGADT
|]

-------------------------------------------------------------------------------
-- Leave
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Leave
  toXMLForGADT
|]

-------------------------------------------------------------------------------
-- Message
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Message
  required
    String
  attributes
    to, String
    from, String
    action, URL
    method, Method
    statusCallback, URL
  recursive
  toXMLForGADT
  toAttrsForAttributes
|]

-------------------------------------------------------------------------------
-- Pause
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Pause
  attributes
    duration, Natural, length
  recursive
  toXMLForGADT
  toAttrsForAttributes
|]

-------------------------------------------------------------------------------
-- Play
-------------------------------------------------------------------------------

type MaybeURL = Maybe URL

twimlSpecStringToData [s|
Play
  required
    MaybeURL
  attributes
    loop, Natural
    digits, Digits
  recursive
  toXMLForGADT
  toAttrsForAttributes
|]

-------------------------------------------------------------------------------
-- Record
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Record
  attributes
    action, URL
    method, Method
    timeout, Natural
    finishOnKey, Key
    maxLength, Natural
    transcribe, Bool
    transcribeCallback, URL
    playBeep, Bool
  recursive
  toXMLForGADT
  toAttrsForAttributes
|]

-------------------------------------------------------------------------------
-- Redirect
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Redirect
  required
    URL
  attributes
    method, Method
  toXMLForGADT
  toAttrsForAttributes
|]

-------------------------------------------------------------------------------
-- Reject
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Reject
  attributes
    reason, Reason
  toXMLForGADT
  toAttrsForAttributes
|]

-------------------------------------------------------------------------------
-- Say
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Say
  required
    String
  attributes
    voice, Voice
    loop, Natural
  recursive
  toXMLForGADT
|]

lang :: Voice -> Maybe (Either Lang LangAlice)
lang (Man   l) = Left  <$> l
lang (Woman l) = Left  <$> l
lang (Alice r) = Right <$> r

instance ToAttrs SayAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr  "voice"      _sayVoice
    , makeAttr  "loop"       _sayLoop
    , makeAttr' "language"  (_sayVoice >=> lang) (either toAttrValue toAttrValue)
    ]

-------------------------------------------------------------------------------
-- Sms
-------------------------------------------------------------------------------

twimlSpecStringToData [s|
Sms
  required
    String
  attributes
    to, String
    from, String
    action, URL
    method, Method
    statusCallback, URL
  recursive
  toXMLForGADT
  toAttrsForAttributes
|]

-------------------------------------------------------------------------------
-- Gather
-------------------------------------------------------------------------------

data Gather

data In

type family Nest a i b where
  Nest i In Gather =
    ( Record   ∉ i
    , Gather   ∉ i
    , Sms      ∉ i
    , Dial     ∉ i
    , Enqueue  ∉ i
    , Leave    ∉ i
    , Hangup   ∉ i
    , Redirect ∉ i
    , Reject   ∉ i
    )

data GatherF i a where
  GatherF :: Nest i In Gather
          => GatherAttributes
          -> IxFree VoiceVerbsF i Void
          -> a
          -> GatherF '[Gather] a

deriving instance Functor (GatherF i)

instance Functor1 GatherF where
  fmap1 = fmap

deriving instance Show a => Show (GatherF i a)

instance ToXML a => ToXML (GatherF i a) where
  toXML (GatherF attrs a b) = makeElement "Gather" (toXML a) (toAttrs attrs) : toXML b

-- | See <https://www.twilio.com/docs/api/twiml/gather#attributes>.
data GatherAttributes = GatherAttributes
  { _gatherAction      :: Maybe URL
  , _gatherMethod      :: Maybe Method
  , _gatherTimeout     :: Maybe Natural
  , _gatherFinishOnKey :: Maybe Key
  , _gatherNumDigits   :: Maybe Natural
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default GatherAttributes where
  def = GatherAttributes
    { _gatherAction      = def
    , _gatherMethod      = def
    , _gatherTimeout     = def
    , _gatherFinishOnKey = def
    , _gatherNumDigits   = def
    }

instance ToAttrs GatherAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "action"      _gatherAction
    , makeAttr "method"      _gatherMethod
    , makeAttr "timeout"     _gatherTimeout
    , makeAttr "finishOnKey" _gatherFinishOnKey
    , makeAttr "numDigits"   _gatherNumDigits
    ]

-------------------------------------------------------------------------------
-- TwiML
-------------------------------------------------------------------------------

data VoiceTwiml = forall i. VoiceTwiml (IxFree VoiceVerbsF i Void)

instance ToElement VoiceTwiml where
  toElement (VoiceTwiml twiml) = unode "Response" $ toXML twiml

instance Show VoiceTwiml where
  show = showTwiml

showTwiml :: VoiceTwiml -> String
showTwiml twiml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ ppElement (toElement twiml) ++ "\n"

newtype VoiceVerbsF i a = VoiceVerbsF
  { getVoiceVerbsF ::
    ( SayF      i :+:
      PlayF     i :+:
      GatherF   i :+:
      SmsF      i :+: -- Shared between Voice and Messaging TwiML
      DialF     i :+:
      EnqueueF  i :+:
      LeaveF    i :+:
      HangupF   i :+:
      RecordF   i :+:
      RedirectF i :+: -- Shared between Voice and Messaging TwiML
      RejectF   i :+:
      PauseF    i :+:
      EndF      i ) a -- Shared between Voice and Messaging TwiML
  } deriving (Functor, Generic, Show, Typeable)

instance
  ( Functor (f i)
  , f i :<: ( SayF      i :+:
              PlayF     i :+:
              GatherF   i :+:
              SmsF      i :+:
              DialF     i :+:
              EnqueueF  i :+:
              LeaveF    i :+:
              HangupF   i :+:
              RecordF   i :+:
              RedirectF i :+:
              RejectF   i :+:
              PauseF    i :+:
              EndF      i )
  ) => f i :<: VoiceVerbsF i where
  inj = VoiceVerbsF . inj
  prj = prj . getVoiceVerbsF

instance Functor1 VoiceVerbsF where
  fmap1 = fmap

instance Show1 VoiceVerbsF where
  show1 = show

instance ToXML a => ToXML (VoiceVerbsF i a) where
  toXML = toXML . getVoiceVerbsF

instance ToXML (IxFree VoiceVerbsF i Void) where
  toXML (IxFree f) = toXML f
  toXML _ = error "Impossible"

data MessagingTwiml = forall i. MessagingTwiml (IxFree MessagingVerbsF i Void)

instance ToElement MessagingTwiml where
  toElement (MessagingTwiml twiml) = unode "Response" $ toXML twiml

instance Show MessagingTwiml where
  show = showMessagingTwiml

showMessagingTwiml :: MessagingTwiml -> String
showMessagingTwiml twiml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ ppElement (toElement twiml) ++ "\n"

newtype MessagingVerbsF i a = MessagingVerbsF
  { getMessagingVerbsF ::
    ( MessageF  i :+:
      RedirectF i :+: -- Shared between Voice and Messaging TwiML
      SmsF      i :+: -- Shared between Voice and Messaging TwiML
      EndF      i ) a -- Shared between Voice and Messaging TwiML
  } deriving (Functor, Generic, Show, Typeable)

instance
  ( Functor (f i)
  , f i :<: ( MessageF  i :+:
              RedirectF i :+:
              SmsF      i :+:
              EndF      i )
  ) => f i :<: MessagingVerbsF i where
  inj = MessagingVerbsF . inj
  prj = prj . getMessagingVerbsF

instance Functor1 MessagingVerbsF where
  fmap1 = fmap

instance Show1 MessagingVerbsF where
  show1 = show

instance ToXML a => ToXML (MessagingVerbsF i a) where
  toXML = toXML . getMessagingVerbsF

instance ToXML (IxFree MessagingVerbsF i Void) where
  toXML (IxFree f) = toXML f
  toXML _ = error "Impossible"

-- | 'Base' maps the empty data declaration for a TwiML verb to its
-- corresponding base functor.
type family Base d where
  Base Dial = DialF
  Base End = EndF
  Base Enqueue = EnqueueF
  Base Gather = GatherF
  Base Hangup = HangupF
  Base Leave = LeaveF
  Base Message = MessageF
  Base Pause = PauseF
  Base Play = PlayF
  Base Record = RecordF
  Base Redirect = RedirectF
  Base Reject = RejectF
  Base Say = SayF
  Base Sms = SmsF

  Base Client = ClientF
  Base Conference = ConferenceF
  Base Number = NumberF
  Base Queue = QueueF
  Base Sip = SipF

type IsTwimlLike f i = (Functor1 f, (Base i) '[i] :<: f '[i])

type TwimlLike f i = TwimlLike' f '[i]

type TwimlLike' f = IxFree f

response :: IxFree VoiceVerbsF i Void -> VoiceTwiml
response = VoiceTwiml

responseMsg :: IxFree MessagingVerbsF i Void -> MessagingTwiml
responseMsg = MessagingTwiml

