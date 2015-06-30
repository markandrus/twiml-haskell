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
{-#LANGUAGE PolyKinds #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE UndecidableInstances #-}

module Text.XML.Twiml.Types where

{-
module Text.XML.Twiml.Types
  ( Natural
  , URL
  , parseURL
  , Method(..)
  , Key(..)
  , Digit(..)
    -- * Phantom Types
  , Say'
  , Play'
  , Gather'
  , Record'
  , Sms'
  , Dial'
  , Enqueue'
  , Leave'
  , Hangup'
  , Redirect'
  , Reject'
  , Pause'
    -- * @\<Say\>@
  , SayAttributes(..)
  , defaultSayAttributes
  , Voice(..)
  , Lang(..)
  , LangAlice(..)
    -- * @\<Play\>@
  , PlayAttributes(..)
  , defaultPlayAttributes
    -- * @\<Gather\>@
  , GatherAttributes(..)
  , defaultGatherAttributes
    -- * @\<Record\>@
  , RecordAttributes(..)
  , defaultRecordAttributes
    -- * @\<Sms\>@
  , SmsAttributes(..)
  , defaultSmsAttributes
    -- * @\<Dial\>@
  , DialAttributes(..)
  , defaultDialAttributes
  , DialNoun(..)
    -- ** @\<Number\>@
  , NumberAttributes(..)
  , defaultNumberAttributes
    -- ** @\<Sip\>@
  , SipAttributes(..)
  , defaultSipAttributes
  , Transport(..)
    -- ** @\<Client\>@
  , ClientAttributes(..)
  , defaultClientAttributes
    -- ** @\<Conference\>@
  , ConferenceAttributes(..)
  , defaultConferenceAttributes
  , ConferenceBeep(..)
    -- ** @\<Queue\>@
  , QueueAttributes(..)
  , defaultQueueAttributes
    -- * @\<Enqueue\>@
  , EnqueueAttributes(..)
  , defaultEnqueueAttributes
    -- * @\<Redirect\>@
  , RedirectAttributes(..)
  , defaultRedirectAttributes
    -- * @\<Reject\>@
  , RejectAttributes(..)
  , defaultRejectAttributes
  , Reason(..)
    -- * @\<Pause\>@
  , PauseAttributes(..)
  , defaultPauseAttributes
    -- * Lens Classes
  , HasLoop(..)
  , HasAction(..)
  , HasMethod(..)
  , HasTimeout(..)
  , HasFinishOnKey(..)
  -- * Internal
    -- * Promoted Lists
    -- ** @(++)@
    -- $promotedLists
  , type (++)
  , SList(..)
  , WitnessList(..)
  , associativity
  , rightIdentity
    -- ** @elem@
    -- $elem
  , Elem
  , type (∉)
    -- * Indexed
    -- ** Functor
    -- $indexedFunctor
  , IxFunctor(..)
    -- ** Applicative
    -- $indexedApplicative
  , IxApplicative(..)
    -- ** Monad
    -- $indexedMonad
  , IxMonad(..)
    -- ** Free
  , IxFree(..)
  , iliftF
    -- ** Show
  , IxShow(..)
  ) where
-}

import Control.DeepSeq (NFData(..))
import Control.Lens hiding (Identity, imap, to)
import Control.Monad
import Data.Data
import Data.Default
import Data.Maybe
import Data.Text hiding (concatMap, map)
import GHC.Generics (Generic)
import Network.URI (URI(..), parseURIReference)
import Text.XML.Light

class ToXML a where
  toXML :: a -> [Element]

class ToElement a where
  toElement :: a -> Element

class ToAttrs a where
  toAttrs :: a -> [Attr]

class ToAttrValue a where
  toAttrValue :: a -> String

instance ToAttrValue Bool where
  toAttrValue True  = "true"
  toAttrValue False = "false"

instance ToAttrValue String where
  toAttrValue = id

{- Attributes -}

-- | See <https://www.twilio.com/docs/api/twiml/say#attributes>.
data SayAttributes = SayAttributes
  { _sayVoice :: Maybe Voice
  , _sayLoop  :: Maybe Natural
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default SayAttributes where
  def = SayAttributes
    { _sayVoice = def
    , _sayLoop  = def
    }

instance ToAttrs SayAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr  "voice"      _sayVoice
    , makeAttr  "loop"       _sayLoop
    , makeAttr' "language"  (_sayVoice >=> lang) (either toAttrValue toAttrValue)
    ]

-- | Voices supported by @\<Say\>@. See
-- <https://www.twilio.com/docs/api/twiml/say#attributes-voice>.
data Voice
  = Man   (Maybe Lang)
  | Woman (Maybe Lang)
  | Alice (Maybe LangAlice)
  deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Voice where
  toAttrValue (Man   _) = "man"
  toAttrValue (Woman _) = "woman"
  toAttrValue (Alice _) = "alice"

lang :: Voice -> Maybe (Either Lang LangAlice)
lang (Man   l) = Left  <$> l
lang (Woman l) = Left  <$> l
lang (Alice r) = Right <$> r

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

-- | See <https://www.twilio.com/docs/api/twiml/play#attributes>.
data PlayAttributes = PlayAttributes
  { _playLoop   :: Maybe Natural
  , _playDigits :: Maybe [Digit]
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default PlayAttributes where
  def = PlayAttributes
    { _playLoop   = def
    , _playDigits = def
    }

instance ToAttrs PlayAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "loop"   _playLoop
    , makeAttr "digits" _playDigits
    ]

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

instance With GatherAttributes where
  with = def

instance ToAttrs GatherAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "action"      _gatherAction
    , makeAttr "method"      _gatherMethod
    , makeAttr "timeout"     _gatherTimeout
    , makeAttr "finishOnKey" _gatherFinishOnKey
    , makeAttr "numDigits"   _gatherNumDigits
    ]

-- | See <https://www.twilio.com/docs/api/twiml/record#attributes>.
data RecordAttributes = RecordAttributes
  { _recordAction             :: Maybe URL
  , _recordMethod             :: Maybe Method
  , _recordTimeout            :: Maybe Natural
  , _recordFinishOnKey        :: Maybe Key
  , _recordMaxLength          :: Maybe Natural
  , _recordTranscribe         :: Maybe Bool
  , _recordTranscribeCallback :: Maybe URL
  , _recordPlayBeep           :: Maybe Bool
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default RecordAttributes where
  def = RecordAttributes
    { _recordAction             = def
    , _recordMethod             = def
    , _recordTimeout            = def
    , _recordFinishOnKey        = def
    , _recordMaxLength          = def
    , _recordTranscribe         = def
    , _recordTranscribeCallback = def
    , _recordPlayBeep           = def
    }

instance With RecordAttributes where
  with = def

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

-- | See <https://www.twilio.com/docs/api/twiml/sms#attributes>.
data SmsAttributes = SmsAttributes
  { _smsTo             :: Maybe String
  , _smsFrom           :: Maybe String
  , _smsAction         :: Maybe URL
  , _smsMethod         :: Maybe Method
  , _smsStatusCallback :: Maybe URL
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default SmsAttributes where
  def = SmsAttributes
    { _smsTo             = def
    , _smsFrom           = def
    , _smsAction         = def
    , _smsMethod         = def
    , _smsStatusCallback = def
    }

instance ToAttrs SmsAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "to"             _smsTo
    , makeAttr "from"           _smsFrom
    , makeAttr "action"         _smsAction
    , makeAttr "method"         _smsMethod
    , makeAttr "statusCallback" _smsStatusCallback
    ]

-- | See <https://www.twilio.com/docs/api/twiml/dial#attributes>.
data DialAttributes = DialAttributes
  { _dialAction       :: Maybe URL
  , _dialMethod       :: Maybe Method
  , _dialTimeout      :: Maybe Natural
  , _dialHangupOnStar :: Maybe Bool
  , _dialTimeLimit    :: Maybe Natural
  , _dialCallerId     :: Maybe String
  , _dialRecord'      :: Maybe Bool
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default DialAttributes where
  def = DialAttributes
    { _dialAction       = def
    , _dialMethod       = def
    , _dialTimeout      = def
    , _dialHangupOnStar = def
    , _dialTimeLimit    = def
    , _dialCallerId     = def
    , _dialRecord'      = def
    }

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

instance With NumberAttributes where
  with = def

instance ToAttrs NumberAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "sendDigits" _numberSendDigits
    , makeAttr "url"        _numberURL
    , makeAttr "method"     _numberMethod
    ]

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

instance With SipAttributes where
  with = def

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

instance With ClientAttributes where
  with = def

instance ToAttrs ClientAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "url"    _clientURL
    , makeAttr "method" _clientMethod
    ]

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

instance With ConferenceAttributes where
  with = def

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

instance With QueueAttributes where
  with = def

instance ToAttrs QueueAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "url"    _queueURL
    , makeAttr "method" _queueMethod
    ]

-- | See <https://www.twilio.com/docs/api/twiml/dial#nouns>.
data DialNoun
  = Number     NumberAttributes     String
  | Sip        SipAttributes        URL    -- NOTE: URL must be under 255 characters.
  | Client     ClientAttributes     String
  | Conference ConferenceAttributes String
  | Queue      QueueAttributes      String
  deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

strToContent :: String -> Content
strToContent str = Text $ CData CDataText str Nothing

txtToContent :: Text -> Content
txtToContent = strToContent . unpack

urlToContent :: URL -> Content
urlToContent (URL url) = strToContent url

makeAttr' :: String -> (a -> Maybe b) -> (b -> String) -> a -> Maybe Attr
makeAttr' str f g a = Attr (unqual str) . g <$> f a

makeAttr :: ToAttrValue b => String -> (a -> Maybe b) -> a -> Maybe Attr
makeAttr str f a = Attr (unqual str) . toAttrValue <$> f a

makeAttrs :: a -> [a -> Maybe Attr] -> [Attr]
makeAttrs a = mapMaybe ($ a)

makeElement :: Node t => String -> t -> [Attr] -> Element
makeElement str c attrs = unode str c & add_attrs attrs

instance ToElement DialNoun where
  toElement (Number     attrs str) = makeElement "Number"     (strToContent str) $ toAttrs attrs
  toElement (Sip        attrs url) = makeElement "Sip"        (urlToContent url) $ toAttrs attrs
  toElement (Client     attrs str) = makeElement "Client"     (strToContent str) $ toAttrs attrs
  toElement (Conference attrs str) = makeElement "Conference" (strToContent str) $ toAttrs attrs
  toElement (Queue      attrs str) = makeElement "Queue"      (strToContent str) $ toAttrs attrs

-- | See <https://www.twilio.com/docs/api/twiml/enqueue#attributes>.
data EnqueueAttributes = EnqueueAttributes
  { _enqueueAction        :: Maybe URL
  , _enqueueMethod        :: Maybe Method
  , _enqueueWaitURL       :: Maybe URL
  , _enqueueWaitURLMethod :: Maybe Method
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default EnqueueAttributes where
  def = EnqueueAttributes
    { _enqueueAction        = def
    , _enqueueMethod        = def
    , _enqueueWaitURL       = def
    , _enqueueWaitURLMethod = def
    }

instance ToAttrs EnqueueAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "action"        _enqueueAction
    , makeAttr "method"        _enqueueMethod
    , makeAttr "waitUrl"       _enqueueWaitURL
    , makeAttr "waitUrlMethod" _enqueueWaitURLMethod
    ]

-- | See <https://www.twilio.com/docs/api/twiml/redirect#attributes>.
data RedirectAttributes = RedirectAttributes
  { _redirectMethod :: Maybe Method
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default RedirectAttributes where
  def = RedirectAttributes
    { _redirectMethod = def
    }

instance ToAttrs RedirectAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "method" _redirectMethod
    ]

-- | See <https://www.twilio.com/docs/api/twiml/reject#attributes>.
data RejectAttributes = RejectAttributes
  { _rejectReason :: Maybe Reason
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default RejectAttributes where
  def = RejectAttributes
    { _rejectReason = def
    }

instance With RejectAttributes where
  with = def

instance ToAttrs RejectAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "reason" _rejectReason
    ]

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

-- | See <https://www.twilio.com/docs/api/twiml/pause#attributes>.
data PauseAttributes = PauseAttributes
  { _pauseDuration :: Maybe Natural
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default PauseAttributes where
  def = PauseAttributes
    { _pauseDuration = def
    }

instance With PauseAttributes where
  with = def

instance ToAttrs PauseAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "length" _pauseDuration
    ]

{- Attribute Lens Classes -}

{- URL, Method & Transport -}

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

data Method = GET | POST
  deriving (Bounded, Data, Enum, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Method where
  toAttrValue = show

type Natural = Int

instance ToAttrValue Natural where
  toAttrValue = show

{- Twiml Datatypes -}

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

{- Voices & Languages -}

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

{- Promoted Lists -}

-- $promotedLists 'IxFree' relies on a promoted list of type constructors, so
-- we'll need
--
-- * promoted list concatenation, @('++')@,
-- * a proof that @('++')@ associates (used by 'iap' and 'ibind'), and
-- * a right identity proof for @[]@ (used by 'iliftF').

{- (++) -}

-- | Promoted list concatenation
type family (++) (a :: [k]) (b :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': as ++ bs

-- | 'SList' is the singleton type for promoted lists.
data SList (i :: [k]) where
  Nil :: SList '[]
  Succ :: SList t -> SList (h ': t)

class WitnessList (xs :: [k]) where
  witness :: SList xs

instance WitnessList '[] where
  witness = Nil

instance WitnessList xs => WitnessList (x ': xs) where
  witness = Succ witness

-- | A proof that @('++')@ associates, i.e.
--
-- @
-- xs ++ (ys ++ zs) ≡ (xs ++ ys) ++ zs
-- @
associativity :: SList xs -> Proxy ys -> Proxy zs
         -> (xs ++ (ys ++ zs)) :~: ((xs ++ ys) ++ zs)
associativity Nil _ _ = Refl
associativity (Succ xs) ys zs =
  case associativity xs ys zs of Refl -> Refl

-- | A proof that
--
-- @
-- xs ≡ xs ++ []
-- @
rightIdentity :: SList xs -> xs :~: (xs ++ '[])
rightIdentity Nil = Refl
rightIdentity (Succ xs) = case rightIdentity xs of Refl -> Refl

{- @elem@ -}

-- $elem 'TwimlF' uses @∉@ in order to enforce nesting rules.

-- | 'Elem' is like a promoted @elem@: it allows us to check whether a type
-- constructor @t@ is present in a list of type constructors @ts@.
type family Elem (t :: k) (ts :: [k]) :: Bool where
  Elem t '[] = 'False
  Elem t (t ': ts) = 'True
  Elem t (u ': ts) = Elem t ts

-- | @t ∉ ts@ is shorthand for asserting that a type constructor @t@ is not
-- present in a list of types constructors @ts@.
type t ∉ ts = Elem t ts ~ 'False

{- Indexed -}

{- Functor -}

-- $indexedFunctor Every 'IxFunctor' is a functor, so you can use @fmap@ and
-- @(\<$>)@ as normal.

-- | An indexed functor @f@
class IxFunctor f where
  -- | The indexed equivalent of @fmap@
  imap :: (a -> b) -> f i a -> f i b

{- Applicative -}

-- $indexedApplicative If you import @Prelude@ hiding @(\<*>)@ and @pure@, you
-- can redefine @(\<*>)@ and @pure@ to use their indexed equivalents. For
-- example,
--
-- @
-- import Prelude hiding ((\<*>), pure)
--
-- pure :: 'IxApplicative' f -> a -> f 'Identity' a
-- pure = 'ipure'
--
-- (\<*>) :: 'IxApplicative' f => f i (a -> b) -> f j a -> f (i '<>' j) b
-- (\<*>) = 'iap'
-- @

-- | An applicative functor @f@ indexed by a monoid @(M,'<>','Identity')@
class IxFunctor f => IxApplicative (f :: k -> * -> *) where
  type Identity :: k

  type (i :: k) <> (j :: k) :: k

  -- | The indexed equivalent of @pure@
  ipure :: a -> f Identity a

  -- | The indexed equivalent of @(\<*>)@
  iap :: f i (a -> b) -> f j a -> f (i <> j) b

{- Monad -}

-- $indexedMonad You can use do-notation with 'IxMonad' by enabling the
-- RebindableSyntax extension and redefining @(>>=)@, @(>>)@, and @return@. For
-- example,
--
-- @
-- {-\#LANGUAGE RebindableSyntax #-}
--
-- import Prelude hiding ((>>=), (>>), return)
--
-- (>>=) :: 'IxMonad' m => m i a -> (a -> m j b) -> m (i '<>' j) b
-- (>>=) = 'ibind'
--
-- (>>) :: 'IxMonad' m => m i a -> m j b -> m (i '<>' j) b
-- a >> b = a >>= const b
--
-- return :: 'IxApplicative' m => a -> m 'Identity' a
-- return = 'ipure'
-- @

-- | A monad @m@ indexed by a monoid @(M,'<>','Identity')@
class IxApplicative m => IxMonad (m :: k -> * -> *) where
  -- | The indexed equivalent of @(>>=)@
  ibind :: m i a -> (a -> m j b) -> m (i <> j) b

{- Free -}

-- | A free monad indexed by a monoid @(M,'++',[])@
data IxFree f (i :: [k]) a where
  IxPure :: a -> IxFree f '[] a
  IxFree :: WitnessList i => f i (IxFree f j a) -> IxFree f (i ++ j) a

instance (IxShow f, Show a) => Show (IxFree f i a) where
  show (IxPure a) = "IxPure (" ++ show a ++ ")"
  show (IxFree fa) = "IxFree (" ++ ishow fa ++ ")"

instance IxShow f => IxShow (IxFree f) where
  ishow = show

instance IxFunctor f => Functor (IxFree f i) where
  fmap = imap

instance IxFunctor f => IxFunctor (IxFree f) where
  imap = fmap

instance IxFunctor f => IxApplicative (IxFree f) where
  type Identity = '[]

  type i <> j = i ++ j

  ipure = IxPure

  iap = iap'

iap'
  :: forall f i j a b. IxFunctor f
  => IxFree f i (a -> b) -> IxFree f j a -> IxFree f (i ++ j) b
iap' (IxPure f) (IxPure a) = IxPure $ f a
iap' (IxPure f) (IxFree mb) = IxFree $ imap (fmap f) mb
iap' (IxFree (mf :: f i1 (IxFree f j1 (a -> b)))) a =
  case associativity (witness :: SList i1) (Proxy :: Proxy j1) (Proxy :: Proxy j)
  of Refl -> IxFree $ imap (`iap'` a) mf

instance (IxFunctor m, IxApplicative (IxFree m)) => IxMonad (IxFree m) where
  ibind = ibind'

ibind'
  :: forall f i j a b. IxFunctor f
  => IxFree f i a -> (a -> IxFree f j b) -> IxFree f (i ++ j) b
ibind' (IxPure a) f = f a
ibind' (IxFree (x :: f i1 (IxFree f j1 a))) f =
    case associativity (witness :: SList i1) (Proxy :: Proxy j1) (Proxy :: Proxy j)
    of Refl -> IxFree $ imap (`ibind'` f) x

-- | Lift an indexed functor into 'IxFree'
iliftF :: forall f i a . (WitnessList i, IxFunctor f) => f i a -> IxFree f i a
iliftF = case rightIdentity (witness :: SList i) of Refl -> IxFree . imap IxPure

class IxNFData (f :: k -> * -> *) where
  irnf :: NFData a => f i a -> ()

instance IxNFData f => IxNFData (IxFree f) where
  irnf = rnf

instance (IxNFData f, NFData a) => NFData (IxFree f i a) where
  rnf (IxPure a) = rnf a
  rnf (IxFree fa) = irnf fa

{- Show -}

class IxShow (f :: k -> * -> *) where
  ishow :: Show a => f i a -> String

{- With -}

class With a where
  with :: a

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
