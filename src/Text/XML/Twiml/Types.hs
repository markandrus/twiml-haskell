{-#LANGUAGE ConstraintKinds #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE DeriveAnyClass #-}
{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE EmptyDataDecls #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE NamedFieldPuns #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverlappingInstances #-}
{-#LANGUAGE PolyKinds #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE UndecidableInstances #-}

module Text.XML.Twiml.Types
  ( Digit(..)
  , Key(..)
  , Method(..)
  , Natural
  , URL
  , parseURL
    -- * TwiMl
  , VoiceTwiml(..)
  , VoiceTwimlF(..)
  , MessagingTwiml(..)
  , MessagingTwimlF(..)
    -- ** Verbs
    -- *** Say
  , Say
  , SayF(..)
  , SayAttributes(..)
  , Voice(..)
  , Lang(..)
  , LangAlice(..)
    -- *** Play
  , Play
  , PlayF(..)
  , PlayAttributes(..)
    -- *** Gather
  , Gather
  , Nest
  , In
  , GatherF(..)
  , GatherAttributes(..)
    -- *** Record
  , Record
  , RecordF(..)
  , RecordAttributes(..)
    -- *** Sms
  , Sms
  , SmsF(..)
  , SmsAttributes(..)
    -- *** Dial
  , Dial
  , DialF(..)
  , DialAttributes(..)
  , DialNoun(..)
    -- **** Number
  , NumberAttributes(..)
    -- **** Sip
  , SipAttributes(..)
  , Transport(..)
    -- **** Client
  , ClientAttributes(..)
    -- *** Conference 
  , ConferenceAttributes(..)
  , ConferenceBeep(..)
    -- **** Queue
  , QueueAttributes(..)
    -- *** Enqueue
  , Enqueue
  , EnqueueF(..)
  , EnqueueAttributes(..)
    -- *** Leave
  , Leave
  , LeaveF(..)
    -- *** Hangup
  , Hangup
  , HangupF(..)
    -- *** Redirect
  , Redirect
  , RedirectF(..)
  , RedirectAttributes(..)
    -- *** Reject
  , Reject
  , RejectF(..)
  , RejectAttributes(..)
  , Reason(..)
    -- *** Pause
  , Pause
  , PauseF(..)
  , PauseAttributes(..)
    -- *** End
  , End
  , EndF(..)
    -- * Lenses
  , HasAction(..)
  , HasBeep(..)
  , HasCallerId(..)
  , HasDigits(..)
  , HasDuration(..)
  , HasEndOnExit(..)
  , HasFinishOnKey(..)
  , HasFrom(..)
  , HasHangupOnStar(..)
  , HasHeaders(..)
  , HasLoop(..)
  , HasMaxLength(..)
  , HasMaxParticipants(..)
  , HasMethod(..)
  , HasMuted(..)
  , HasNumDigits(..)
  , HasPassword(..)
  , HasPlayBeep(..)
  , HasReason(..)
  , HasRecord'(..)
  , HasSendDigits(..)
  , HasStartOnEnter(..)
  , HasStatusCallback(..)
  , HasTimeout(..)
  , HasTimeLimit(..)
  , HasTo(..)
  , HasTranscribe(..)
  , HasTranscribeCallback(..)
  , HasTransport(..)
  , HasURL(..)
  , HasVoice(..)
  , HasWaitMethod(..)
  , HasWaitURL(..)
  ) where

import Control.DeepSeq (NFData(..))
import Control.Lens hiding (Identity, to)
import Control.Monad
import Data.Data
import Data.Default
import Data.Maybe
import Data.Void
import GHC.Generics (Generic)
import Network.URI (URI(..), parseURIReference)
import Text.XML.Light

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.TH

-- | The ‘digits’ attribute lets you play DTMF tones during a call. See
-- <https://www.twilio.com/docs/api/twiml/play#attributes-digits>.
data Digit
  = D0 -- ^ 0
  | D1 -- ^ 1
  | D2 -- ^ 2
  | D3 -- ^ 3
  | D4 -- ^ 4
  | D5 -- ^ 5
  | D6 -- ^ 6
  | D7 -- ^ 7
  | D8 -- ^ 8
  | D9 -- ^ 9
  | W  -- ^ w
  deriving (Bounded, Data, Enum, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Digit where
  toAttrValue D0 = "0"
  toAttrValue D1 = "1"
  toAttrValue D2 = "2"
  toAttrValue D3 = "3"
  toAttrValue D4 = "4"
  toAttrValue D5 = "5"
  toAttrValue D6 = "6"
  toAttrValue D7 = "7"
  toAttrValue D8 = "8"
  toAttrValue D9 = "9"
  toAttrValue W  = "w"

instance ToAttrValue [Digit] where
  toAttrValue = concatMap toAttrValue

data Key
  = K0      -- ^ 0
  | K1      -- ^ 1
  | K2      -- ^ 2
  | K3      -- ^ 3
  | K4      -- ^ 4
  | K5      -- ^ 5
  | K6      -- ^ 6
  | K7      -- ^ 7
  | K8      -- ^ 8
  | K9      -- ^ 9
  | KStar   -- ^ \*
  | KPound  -- ^ #
  deriving (Bounded, Data, Enum, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Key where
  toAttrValue K0     = "0"
  toAttrValue K1     = "1"
  toAttrValue K2     = "2"
  toAttrValue K3     = "3"
  toAttrValue K4     = "4"
  toAttrValue K5     = "5"
  toAttrValue K6     = "6"
  toAttrValue K7     = "7"
  toAttrValue K8     = "8"
  toAttrValue K9     = "9"
  toAttrValue KStar  = "*"
  toAttrValue KPound = "#"

type Natural = Int

instance ToAttrValue Natural where
  toAttrValue = show

data Method = GET | POST
  deriving (Bounded, Data, Enum, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Method where
  toAttrValue = show

newtype URL = URL { getURL :: String }
  deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue URL where
  toAttrValue = getURL

-- | Checks whether a @URI@'s scheme, if any, is one of @"http:"@ or @"https:"@.
isHttp :: URI -> Bool
isHttp uri = case uriScheme uri of
  ""       -> True
  "http:"  -> True
  "https:" -> True
  _        -> False

parseURL :: String -> Maybe URL
parseURL url = parseURIReference url
           >>= (\uri -> if isHttp uri then Just (URL url) else Nothing)

-- | Voices supported by @\<Say\>@. See
-- <https://www.twilio.com/docs/api/twiml/say#attributes-voice>.
data Voice
  = Man   (Maybe Lang)
  | Woman (Maybe Lang)
  | Alice (Maybe LangAlice)
  deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

-- | Languages spoken by voices 'Man' and 'Woman'. See
-- <https://www.twilio.com/docs/api/twiml/say#attributes-manwoman>.
data Lang
  = English
  | EnglishUK
  | Spanish
  | French
  | German
  | Italian
  deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Lang where
  toAttrValue English   = "en"
  toAttrValue EnglishUK = "en-gb"
  toAttrValue Spanish   = "es"
  toAttrValue French    = "fr"
  toAttrValue German    = "de"
  toAttrValue Italian   = "it"

-- | Languages spoken by 'Alice'. See
-- <https://www.twilio.com/docs/api/twiml/say#attributes-alice>.
data LangAlice
  = DaDK -- ^ Danish, Denmark
  | DeDE -- ^ German, Germany
  | EnAU -- ^ English, Australia
  | EnCA -- ^ English, Canada
  | EnGB -- ^ English, UK
  | EnIN -- ^ English, India
  | EnUS -- ^ English, United States
  | CaES -- ^ Catalan, Spain
  | EsES -- ^ Spanish, Spain
  | EsMX -- ^ Spanish, Mexico
  | FiFI -- ^ Finnish, Finland
  | FrCA -- ^ French, Canada
  | FrFR -- ^ French, France
  | ItIT -- ^ Italian, Italy
  | JaJP -- ^ Japanese, Japan
  | KoKR -- ^ Korean, Korea
  | NbNO -- ^ Norwegian, Norway
  | NlNL -- ^ Dutch, Netherlands
  | PlPL -- ^ Polish-Poland
  | PtBR -- ^ Portuguese, Brazil
  | PtPT -- ^ Portuguese, Portugal
  | RuRU -- ^ Russian, Russia
  | SvSE -- ^ Swedish, Sweden
  | ZhCN -- ^ Chinese (Mandarin)
  | ZhHK -- ^ Chinese (Cantonese)
  | ZhTW -- ^ Chinese (Taiwanese Mandarin)
  deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue LangAlice where
  toAttrValue DaDK = "da-DK"
  toAttrValue DeDE = "de-DE"
  toAttrValue EnAU = "en-AU"
  toAttrValue EnCA = "en-CA"
  toAttrValue EnGB = "en-GB"
  toAttrValue EnIN = "en-IN"
  toAttrValue EnUS = "en-US"
  toAttrValue CaES = "ca-ES"
  toAttrValue EsES = "es-ES"
  toAttrValue EsMX = "es-MX"
  toAttrValue FiFI = "fi-FI"
  toAttrValue FrCA = "fr-CA"
  toAttrValue FrFR = "fr-FR"
  toAttrValue ItIT = "it-IT"
  toAttrValue JaJP = "ja-JP"
  toAttrValue KoKR = "ko-KR"
  toAttrValue NbNO = "nb-NO"
  toAttrValue NlNL = "nl-NL"
  toAttrValue PlPL = "pl-PL"
  toAttrValue PtBR = "pt-BR"
  toAttrValue PtPT = "pt-PT"
  toAttrValue RuRU = "ru-RU"
  toAttrValue SvSE = "sv-SE"
  toAttrValue ZhCN = "zh-CN"
  toAttrValue ZhHK = "zh-HK"
  toAttrValue ZhTW = "zh-TW"

type MURL = Maybe URL
type Digits = [Digit]

twimlSpecStringToData "Say\n\
\  required\n\
\    String\n\
\  attributes\n\
\    voice, Voice\n\
\    loop, Natural\n\
\  recursive\n"

twimlSpecStringToData "Play\n\
\  required\n\
\    MURL\n\
\  attributes\n\
\    loop, Natural\n\
\    digits, Digits\n\
\  recursive\n"

twimlSpecStringToData "Record\n\
\  attributes\n\
\    action, URL\n\
\    method, Method\n\
\    timeout, Natural\n\
\    finishOnKey, Key\n\
\    maxLength, Natural\n\
\    transcribe, Bool\n\
\    transcribeCallback, URL\n\
\    playBeep, Bool\n\
\  recursive\n"

twimlSpecStringToData "Message\n\
\  required\n\
\    String\n\
\  attributes\n\
\    to, String\n\
\    from, String\n\
\    action, URL\n\
\    method, Method\n\
\    statusCallback, URL\n\
\  recursive\n"

twimlSpecStringToData "Sms\n\
\  required\n\
\    String\n\
\  attributes\n\
\    to, String\n\
\    from, String\n\
\    action, URL\n\
\    method, Method\n\
\    statusCallback, URL\n\
\  recursive\n"

{- Number -}

-- | See <https://www.twilio.com/docs/api/twiml/number#attributes>.
data NumberAttributes = NumberAttributes
  { _numberSendDigits :: Maybe [Digit]
  , _numberURL        :: Maybe URL
  , _numberMethod     :: Maybe Method
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default NumberAttributes where
  def = NumberAttributes
    { _numberSendDigits = def
    , _numberURL        = def
    , _numberMethod     = def
    }

instance ToAttrs NumberAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "sendDigits" _numberSendDigits
    , makeAttr "url"        _numberURL
    , makeAttr "method"     _numberMethod
    ]

{- Sip -}

-- | See <https://www.twilio.com/docs/api/twiml/sip#attributes>.
data SipAttributes = SipAttributes
  { _sipUsername  :: Maybe String
  , _sipPassword  :: Maybe String
  , _sipTransport :: Maybe Transport
  , _sipHeaders   :: Maybe String    -- NOTE: Under 1024 characters.
  , _sipURL       :: Maybe URL
  , _sipMethod    :: Maybe Method
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default SipAttributes where
  def = SipAttributes
    { _sipUsername  = def
    , _sipPassword  = def
    , _sipTransport = def
    , _sipHeaders   = def
    , _sipURL       = def
    , _sipMethod    = def
    }

instance ToAttrs SipAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "username"  _sipUsername
    , makeAttr "password"  _sipPassword
    , makeAttr "transport" _sipTransport
    , makeAttr "headers"   _sipHeaders
    , makeAttr "url"       _sipURL
    , makeAttr "method"    _sipMethod
    ]

-- | See <https://www.twilio.com/docs/api/twiml/sip#transport>.
data Transport = TCP | UDP
  deriving (Bounded, Data, Enum, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Transport where
  toAttrValue TCP = "tcp"
  toAttrValue UDP = "udp"

{- Client -}

-- | See <https://www.twilio.com/docs/api/twiml/client#attributes>.
data ClientAttributes = ClientAttributes
  { _clientURL    :: Maybe URL
  , _clientMethod :: Maybe Method
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default ClientAttributes where
  def = ClientAttributes
    { _clientURL    = def
    , _clientMethod = def
    }

instance ToAttrs ClientAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "url"    _clientURL
    , makeAttr "method" _clientMethod
    ]

{- Conference -}

-- | See <https://www.twilio.com/docs/api/twiml/conference#attributes>.
data ConferenceAttributes = ConferenceAttributes
  { _conferenceMuted           :: Maybe Bool
  , _conferenceBeep            :: Maybe Bool
  , _conferenceStartOnEnter    :: Maybe Bool
  , _conferenceEndOnExit       :: Maybe Bool
  , _conferenceWaitURL         :: Maybe URL
  , _conferenceWaitMethod      :: Maybe Method
  , _conferenceMaxParticipants :: Maybe Natural -- FIXME: Non-zero, less than 40.
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default ConferenceAttributes where
  def = ConferenceAttributes
    { _conferenceMuted           = def
    , _conferenceBeep            = def
    , _conferenceStartOnEnter    = def
    , _conferenceEndOnExit       = def
    , _conferenceWaitURL         = def
    , _conferenceWaitMethod      = def
    , _conferenceMaxParticipants = def
    }

instance ToAttrs ConferenceAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "muted"                  _conferenceMuted
    , makeAttr "beep"                   _conferenceBeep
    , makeAttr "startConferenceOnEnter" _conferenceStartOnEnter
    , makeAttr "endConferenceOnExit"    _conferenceEndOnExit
    , makeAttr "waitURL"                _conferenceWaitURL
    , makeAttr "waitMethod"             _conferenceWaitMethod
    , makeAttr "maxParticipants"        _conferenceMaxParticipants
    ]

-- | See <https://www.twilio.com/docs/api/twiml/conference#attributes-beep>.
data ConferenceBeep
  = Yes
  | No
  | OnExit
  | OnEnter
  deriving (Bounded, Data, Enum, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue ConferenceBeep where
  toAttrValue Yes     = "yes"
  toAttrValue No      = "no"
  toAttrValue OnExit  = "on-exit"
  toAttrValue OnEnter = "on-enter"

{- Queue -}

-- | See <https://www.twilio.com/docs/api/twiml/queue#attributes>.
data QueueAttributes = QueueAttributes
  { _queueURL    :: Maybe URL
  , _queueMethod :: Maybe Method
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default QueueAttributes where
  def = QueueAttributes
    { _queueURL    = def
    , _queueMethod = def
    }

instance ToAttrs QueueAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "url"    _queueURL
    , makeAttr "method" _queueMethod
    ]

data SomeNode = forall n. Node n => SomeNode n

class ToSomeNode a where
  toSomeNode :: a -> SomeNode

instance ToSomeNode a => ToSomeNode (Maybe a) where
  toSomeNode (Just a) = toSomeNode a
  toSomeNode _ = SomeNode ()

instance Node SomeNode where
  node qName (SomeNode n) = node qName n

instance ToSomeNode n => Node n where
  node qName n = node qName (toSomeNode n)

instance ToSomeNode String where
  toSomeNode str = SomeNode . Text $ CData CDataText str Nothing

instance ToSomeNode URL where
  toSomeNode = toSomeNode . getURL

instance ToSomeNode () where
  toSomeNode = SomeNode

makeAttr' :: String -> (a -> Maybe b) -> (b -> String) -> a -> Maybe Attr
makeAttr' str f g a = Attr (unqual str) . g <$> f a

makeAttr :: ToAttrValue b => String -> (a -> Maybe b) -> a -> Maybe Attr
makeAttr str f a = Attr (unqual str) . toAttrValue <$> f a

makeAttrs :: a -> [a -> Maybe Attr] -> [Attr]
makeAttrs a = mapMaybe ($ a)

makeElement :: Node t => String -> t -> [Attr] -> Element
makeElement str c attrs = unode str c & add_attrs attrs

instance ToElement DialNoun where
  toElement (Number     attrs str) = makeElement "Number"     (toSomeNode str) $ toAttrs attrs
  toElement (Sip        attrs url) = makeElement "Sip"        (toSomeNode url) $ toAttrs attrs
  toElement (Client     attrs str) = makeElement "Client"     (toSomeNode str) $ toAttrs attrs
  toElement (Conference attrs str) = makeElement "Conference" (toSomeNode str) $ toAttrs attrs
  toElement (Queue      attrs str) = makeElement "Queue"      (toSomeNode str) $ toAttrs attrs


-- | See <https://www.twilio.com/docs/api/twiml/dial#nouns>.
data DialNoun
  = Number     NumberAttributes     String
  | Sip        SipAttributes        URL    -- NOTE: URL must be under 255 characters.
  | Client     ClientAttributes     String
  | Conference ConferenceAttributes String
  | Queue      QueueAttributes      String
  deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

-- FIXME(mroberts):
type EDS = Either DialNoun String

instance Default EDS where
  def = Right def

twimlSpecStringToData "Dial\n\
\  required\n\
\    EDS\n\
\  attributes\n\
\    action, URL\n\
\    method, Method\n\
\    timeout, Natural\n\
\    hangupOnStar, Bool\n\
\    timeLimit, Natural\n\
\    callerId, String\n\
\    record', Bool, record\n\
\  recursive\n"

twimlSpecStringToData "Enqueue\n\
\  required\n\
\    String\n\
\  attributes\n\
\    action, URL\n\
\    method, Method\n\
\    waitURL, URL, waitUrl\n\
\    waitMethod, Method, waitUrlMethod\n\
\  recursive\n"

twimlSpecStringToData "Leave\n"

twimlSpecStringToData "Hangup\n"

twimlSpecStringToData "Redirect\n\
\  required\n\
\    URL\n\
\  attributes\n\
\    method, Method\n"

twimlSpecStringToData "Reject\n\
\  attributes\n\
\    reason, Reason\n"

-- | The reason attribute takes the values \"rejected\" and \"busy.\" This tells
-- Twilio what message to play when rejecting a call. Selecting \"busy\" will
-- play a busy signal to the caller, while selecting \"rejected\" will play a
-- standard not-in-service response.
-- See <https://www.twilio.com/docs/api/twiml/reject#attributes-reason>.
data Reason = Rejected | Busy
  deriving (Bounded, Data, Enum, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Reason where
  toAttrValue Rejected = "rejected"
  toAttrValue Busy     = "busy"

twimlSpecStringToData "Pause\n\
\  attributes\n\
\    duration, Natural, length\n\
\  recursive\n"

twimlSpecStringToData "End\n"

{- TwiML -}

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

{- Verbs -}

{- Say -}

instance ToXML a => ToXML (SayF i a) where
  toXML (SayF a attrs b) = makeElement "Say" (toSomeNode a) (toAttrs attrs) : toXML b

instance ToAttrs SayAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr  "voice"      _sayVoice
    , makeAttr  "loop"       _sayLoop
    , makeAttr' "language"  (_sayVoice >=> lang) (either toAttrValue toAttrValue)
    ]

instance ToAttrValue Voice where
  toAttrValue (Man   _) = "man"
  toAttrValue (Woman _) = "woman"
  toAttrValue (Alice _) = "alice"

lang :: Voice -> Maybe (Either Lang LangAlice)
lang (Man   l) = Left  <$> l
lang (Woman l) = Left  <$> l
lang (Alice r) = Right <$> r

{- Play -}

instance ToXML a => ToXML (PlayF i a) where
  toXML (PlayF a attrs b) = makeElement "Play" (toSomeNode a) (toAttrs attrs) : toXML b

instance ToAttrs PlayAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "loop"   _playLoop
    , makeAttr "digits" _playDigits
    ]

{- Gather -}

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

{- Record -}

instance ToXML a => ToXML (RecordF i a) where
  toXML (RecordF attrs a) = makeElement "Record" () (toAttrs attrs) : toXML a

instance ToAttrs RecordAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "action"             _recordAction
    , makeAttr "method"             _recordMethod
    , makeAttr "timeout"            _recordTimeout
    , makeAttr "finishOnKey"        _recordFinishOnKey
    , makeAttr "maxLength"          _recordMaxLength
    , makeAttr "transcribe"         _recordTranscribe
    , makeAttr "transcribeCallback" _recordTranscribeCallback
    , makeAttr "playBeep"           _recordPlayBeep
    ]

{- Sms -}

instance ToXML a => ToXML (MessageF i a) where
  toXML (MessageF a attrs b) = makeElement "Message" (toSomeNode a) (toAttrs attrs) : toXML b

instance ToAttrs MessageAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "to"             _messageTo
    , makeAttr "from"           _messageFrom
    , makeAttr "action"         _messageAction
    , makeAttr "method"         _messageMethod
    , makeAttr "statusCallback" _messageStatusCallback
    ]

instance ToXML a => ToXML (SmsF i a) where
  toXML (SmsF a attrs b) = makeElement "Sms" (toSomeNode a) (toAttrs attrs) : toXML b

instance ToAttrs SmsAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "to"             _smsTo
    , makeAttr "from"           _smsFrom
    , makeAttr "action"         _smsAction
    , makeAttr "method"         _smsMethod
    , makeAttr "statusCallback" _smsStatusCallback
    ]

{- Dial -}

instance ToSomeNode (Either DialNoun String) where
  toSomeNode (Left  a) = SomeNode $ toElement a
  toSomeNode (Right a) = toSomeNode a

instance ToXML a => ToXML (DialF i a) where
  toXML (DialF a attrs b) = makeElement "Dial" (toSomeNode a) (toAttrs attrs) : toXML b

instance ToAttrs DialAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "action"       _dialAction
    , makeAttr "method"       _dialMethod
    , makeAttr "timeout"      _dialTimeout
    , makeAttr "hangupOnStar" _dialHangupOnStar
    , makeAttr "timeLimit"    _dialTimeLimit
    , makeAttr "callerId"     _dialCallerId
    , makeAttr "record"       _dialRecord'
    ]

{- Enqueue -}

instance ToXML a => ToXML (EnqueueF i a) where
  toXML (EnqueueF a attrs b) = makeElement "Enqueue" (toSomeNode a) (toAttrs attrs) : toXML b

instance ToAttrs EnqueueAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "action"        _enqueueAction
    , makeAttr "method"        _enqueueMethod
    , makeAttr "waitUrl"       _enqueueWaitURL
    , makeAttr "waitUrlMethod" _enqueueWaitMethod
    ]

{- Leave -}

instance ToXML (LeaveF i a) where
  toXML LeaveF = [makeElement "Leave" () []]

{- Hangup -}

instance ToXML (HangupF i a) where
  toXML HangupF = [makeElement "Hangup" () []]

{- Redirect -}

instance ToXML (RedirectF i a) where
  toXML (RedirectF a attrs) = [makeElement "Redirect" (toSomeNode a) $ toAttrs attrs]

instance ToAttrs RedirectAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "method" _redirectMethod
    ]

{- Reject -}

instance ToXML (RejectF i a) where
  toXML (RejectF attrs) = [makeElement "Reject" () $ toAttrs attrs]

instance ToAttrs RejectAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "reason" _rejectReason
    ]

{- Pause -}

instance ToXML a => ToXML (PauseF i a) where
  toXML (PauseF attrs a) = makeElement "Pause" () (toAttrs attrs) : toXML a

instance ToAttrs PauseAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "length" _pauseDuration
    ]

{- End -}

instance ToXML (EndF i a) where
  toXML EndF = []

{- Elem -}

-- $elem 'TwimlF uses @∉@ in order to enforce nesting rules.

-- | 'Elem' is like a promoted @elem@: it allows us to check whether a type
-- constructor @t@ is present in a list of type constructors @ts@.
type family Elem (t :: k) (ts :: [k]) :: Bool where
  Elem t '[] = 'False
  Elem t (t ': ts) = 'True
  Elem t (u ': ts) = Elem t ts

{- @(∉)@ -}

-- | @t ∉ ts@ is shorthand for asserting that a type constructor @t@ is not
-- present in a list of types constructors @ts@.
type t ∉ ts = Elem t ts ~ 'False

{- Lenses -}

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
