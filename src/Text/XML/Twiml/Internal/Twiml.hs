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
  , MessagingTwimlF(..)
  , VoiceTwiml(..)
  , VoiceTwimlF(..)
  , Base
  , IsTwimlLike
  , TwimlLike
  , TwimlLike'
  , response
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

type Digits = [Digit]
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
          -> IxFree VoiceTwimlF i Void
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

data VoiceTwiml = forall i. VoiceTwiml (IxFree VoiceTwimlF i Void)

instance ToElement VoiceTwiml where
  toElement (VoiceTwiml twiml) = unode "Response" $ toXML twiml

instance Show VoiceTwiml where
  show = showTwiml

showTwiml :: VoiceTwiml -> String
showTwiml twiml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ ppElement (toElement twiml) ++ "\n"

newtype VoiceTwimlF i a = VoiceTwimlF
  { getVoiceTwimlF ::
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

instance (f i :<: ( SayF      i :+:
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
         ) => f i :<: VoiceTwimlF i where
  inj = VoiceTwimlF . inj
  prj = prj . getVoiceTwimlF

instance Functor1 VoiceTwimlF where
  fmap1 = fmap

instance Show1 VoiceTwimlF where
  show1 = show

instance ToXML a => ToXML (VoiceTwimlF i a) where
  toXML = toXML . getVoiceTwimlF

instance ToXML (IxFree VoiceTwimlF i Void) where
  toXML (IxFree f) = toXML f
  toXML _ = error "Impossible"

data MessagingTwiml = forall i. MessagingTwiml (IxFree MessagingTwimlF i Void)

instance ToElement MessagingTwiml where
  toElement (MessagingTwiml twiml) = unode "Response" $ toXML twiml

instance Show MessagingTwiml where
  show = showMessagingTwiml

showMessagingTwiml :: MessagingTwiml -> String
showMessagingTwiml twiml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ ppElement (toElement twiml) ++ "\n"

newtype MessagingTwimlF i a = MessagingTwimlF
  { getMessagingTwimlF ::
    ( MessageF  i :+:
      RedirectF i :+: -- Shared between Voice and Messaging TwiML
      SmsF      i :+: -- Shared between Voice and Messaging TwiML
      EndF      i ) a -- Shared between Voice and Messaging TwiML
  } deriving (Functor, Generic, Show, Typeable)

instance (f i :<: ( MessageF  i :+:
                    RedirectF i :+:
                    SmsF      i :+:
                    EndF      i )
         ) => f i :<: MessagingTwimlF i where
  inj = MessagingTwimlF . inj
  prj = prj . getMessagingTwimlF

instance Functor1 MessagingTwimlF where
  fmap1 = fmap

instance Show1 MessagingTwimlF where
  show1 = show

instance ToXML a => ToXML (MessagingTwimlF i a) where
  toXML = toXML . getMessagingTwimlF

instance ToXML (IxFree MessagingTwimlF i Void) where
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

type IsTwimlLike f i = (Functor1 f, (Base i) '[i] :<: f '[i])

type TwimlLike f i = TwimlLike' f '[i]

type TwimlLike' f = IxFree f

response :: IxFree VoiceTwimlF i Void -> VoiceTwiml
response = VoiceTwiml
