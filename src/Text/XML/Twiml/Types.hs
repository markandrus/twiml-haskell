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
  , Say
  , SayF(..)
  , SayAttributes(..)
  , defaultSayAttributes
  , Voice(..)
  , Lang(..)
  , LangAlice(..)
    -- * @\<Play\>@
  , Play
  , PlayF
  , PlayAttributes(..)
  , defaultPlayAttributes
    -- * @\<Gather\>@
  , Gather
  , GatherF
  , GatherAttributes(..)
  , defaultGatherAttributes
    -- * @\<Record\>@
  , Record
  , RecordF
  , RecordAttributes(..)
  , defaultRecordAttributes
    -- * @\<Sms\>@
  , Sms
  , SmsF
  , SmsAttributes(..)
  , defaultSmsAttributes
    -- * @\<Dial\>@
  , Dial
  , DialF
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
  , Enqueue
  , EnqueueF
  , EnqueueAttributes(..)
  , defaultEnqueueAttributes
    -- * @\<Redirect\>@
  , Redirect
  , RedirectF
  , RedirectAttributes(..)
  , defaultRedirectAttributes
    -- * @\<Reject\>@
  , Reject
  , RejectF
  , RejectAttributes(..)
  , defaultRejectAttributes
  , Reason(..)
    -- * @\<Pause\>@
  , Pause
  , PauseF
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
  , Functor1(..)
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
  , Show1(..)
  ) where
-}

import Control.DeepSeq (NFData(..))
import Control.Lens hiding (Identity, fmap1, to)
import Control.Monad
import Data.Data
import Data.Default
import Data.Maybe
import Data.Text hiding (concatMap, map)
import Data.Void
import GHC.Generics (Generic)
import Network.URI (URI(..), parseURIReference)
import Text.XML.Light

import Text.XML.Twiml.Internal

{- Attributes -}

data Say

data SayF i a where
  SayF :: String -> SayAttributes -> a -> SayF '[Say] a

deriving instance Data a => Data (SayF '[Say] a)

deriving instance Eq a => Eq (SayF i a)

deriving instance Functor (SayF i)

instance Functor1 SayF where
  fmap1 = fmap

instance NFData a => NFData (SayF i a) where
  rnf (SayF a b c) = rnf a `seq` rnf b `seq` rnf c

deriving instance Ord a => Ord (SayF i a)

deriving instance Read a => Read (SayF '[Say] a)

deriving instance Show a => Show (SayF i a)

instance ToXML a => ToXML (SayF i a) where
  toXML (SayF a attrs b) = makeElement "Say" (strToContent a) (toAttrs attrs) : toXML b

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

data Play

data PlayF i a where
  PlayF :: Maybe URL -> PlayAttributes -> a -> PlayF '[Play] a

deriving instance Data a => Data (PlayF '[Play] a)

deriving instance Eq a => Eq (PlayF i a)

deriving instance Functor (PlayF i)

instance Functor1 PlayF where
  fmap1 = fmap

instance NFData a => NFData (PlayF i a) where
  rnf (PlayF a b c) = rnf a `seq` rnf b `seq` rnf c

deriving instance Ord a => Ord (PlayF i a)

deriving instance Read a => Read (PlayF '[Play] a)

deriving instance Show a => Show (PlayF i a)

instance ToXML a => ToXML (PlayF i a) where
  toXML (PlayF (Just a) attrs b) = makeElement "Play" (urlToContent a) (toAttrs attrs) : toXML b
  toXML (PlayF _        attrs b) = makeElement "Play" ()               (toAttrs attrs) : toXML b

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
           -> VoiceTwiml' i Void
           -> a
           -> GatherF '[Gather] a

-- deriving instance Data a => Data (GatherF '[Gather] a)

-- deriving instance Eq a => Eq (GatherF i a)

deriving instance Functor (GatherF i)

instance Functor1 GatherF where
  fmap1 = fmap

-- deriving instance Read a => Read (GatherF '[Gather] a)

-- deriving instance Ord a => Ord (GatherF i a)

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

data Record

data RecordF i a where
  RecordF :: RecordAttributes -> a -> RecordF '[Record] a

deriving instance Data a => Data (RecordF '[Record] a)

deriving instance Eq a => Eq (RecordF i a)

deriving instance Functor (RecordF i)

instance Functor1 RecordF where
  fmap1 = fmap

instance NFData a => NFData (RecordF i a) where
  rnf (RecordF a b) = rnf a `seq` rnf b

deriving instance Ord a => Ord (RecordF i a)

deriving instance Read a => Read (RecordF '[Record] a)

deriving instance Show a => Show (RecordF i a)

instance ToXML a => ToXML (RecordF i a) where
  toXML (RecordF attrs a) = makeElement "Record" () (toAttrs attrs) : toXML a

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

data Sms

data SmsF i a where
  SmsF :: String -> SmsAttributes -> a -> SmsF '[Sms] a

deriving instance Data a => Data (SmsF '[Sms] a)

deriving instance Eq a => Eq (SmsF i a)

deriving instance Functor (SmsF i)

instance Functor1 SmsF where
  fmap1 = fmap

instance NFData a => NFData (SmsF i a) where
  rnf (SmsF a b c) = rnf a `seq` rnf b `seq` rnf c

deriving instance Ord a => Ord (SmsF i a)

deriving instance Read a => Read (SmsF '[Sms] a)

deriving instance Show a => Show (SmsF i a)

instance ToXML a => ToXML (SmsF i a) where
  toXML (SmsF a attrs b) = makeElement "Sms" (strToContent a) (toAttrs attrs) : toXML b

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

data Dial

data DialF i a where
  DialF :: Either DialNoun String -> DialAttributes -> a -> DialF '[Dial] a

deriving instance Data a => Data (DialF '[Dial] a)

deriving instance Eq a => Eq (DialF i a)

deriving instance Functor (DialF i)

instance Functor1 DialF where
  fmap1 = fmap

instance NFData a => NFData (DialF i a) where
  rnf (DialF a b c) = rnf a `seq` rnf b `seq` rnf c

deriving instance Ord a => Ord (DialF i a)

deriving instance Read a => Read (DialF '[Dial] a)

deriving instance Show a => Show (DialF i a)

instance ToXML a => ToXML (DialF i a) where
  toXML (DialF (Left  a) attrs b) = makeElement "Dial" (toElement    a) (toAttrs attrs) : toXML b
  toXML (DialF (Right a) attrs b) = makeElement "Dial" (strToContent a) (toAttrs attrs) : toXML b

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

data Enqueue

data EnqueueF i a where
  EnqueueF :: String -> EnqueueAttributes -> a -> EnqueueF '[Enqueue] a

deriving instance Data a => Data (EnqueueF '[Enqueue] a)

deriving instance Eq a => Eq (EnqueueF i a)

deriving instance Functor (EnqueueF i)

instance Functor1 EnqueueF where
  fmap1 = fmap

instance NFData a => NFData (EnqueueF i a) where
  rnf (EnqueueF a b c) = rnf a `seq` rnf b `seq` rnf c

deriving instance Ord a => Ord (EnqueueF i a)

deriving instance Read a => Read (EnqueueF '[Enqueue] a)

deriving instance Show a => Show (EnqueueF i a)

instance ToXML a => ToXML (EnqueueF i a) where
  toXML (EnqueueF a attrs b) = makeElement "Enqueue" (strToContent a) (toAttrs attrs) : toXML b

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

data Leave

data LeaveF i a where
  LeaveF :: LeaveF '[Leave] a

deriving instance Data a => Data (LeaveF '[Leave] a)

deriving instance Eq (LeaveF i a)

deriving instance Functor (LeaveF i)

instance Functor1 LeaveF where
  fmap1 = fmap

instance NFData (LeaveF i a) where
  rnf LeaveF = ()

deriving instance Ord (LeaveF i a)

deriving instance Read (LeaveF '[Leave] a)

deriving instance Show (LeaveF i a)

instance ToXML (LeaveF i a) where
  toXML LeaveF = [makeElement "Leave" () []]

data Hangup

data HangupF i a where
  HangupF :: HangupF '[Hangup] a

deriving instance Data a => Data (HangupF '[Hangup] a)

deriving instance Eq (HangupF i a)

deriving instance Functor (HangupF i)

instance Functor1 HangupF where
  fmap1 = fmap

instance NFData (HangupF i a) where
  rnf HangupF = ()

deriving instance Ord (HangupF i a)

deriving instance Read (HangupF '[Hangup] a)

deriving instance Show (HangupF i a)

instance ToXML (HangupF i a) where
  toXML HangupF = [makeElement "Hangup" () []]

data Redirect

data RedirectF i a where
  RedirectF :: URL -> RedirectAttributes -> RedirectF '[Redirect] a

deriving instance Data a => Data (RedirectF '[Redirect] a)

deriving instance Eq a => Eq (RedirectF i a)

deriving instance Functor (RedirectF i)

instance Functor1 RedirectF where
  fmap1 = fmap

instance NFData (RedirectF i a) where
  rnf (RedirectF a b) = rnf a `seq` rnf b

deriving instance Ord a => Ord (RedirectF i a)

deriving instance Read a => Read (RedirectF '[Redirect] a)

deriving instance Show a => Show (RedirectF i a)

instance ToXML (RedirectF i a) where
  toXML (RedirectF a attrs) = [makeElement "Redirect" (urlToContent a) $ toAttrs attrs]

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

data Reject

data RejectF i a where
  RejectF :: RejectAttributes -> RejectF '[Reject] a

deriving instance Data a => Data (RejectF '[Reject] a)

deriving instance Eq (RejectF i a)

deriving instance Functor (RejectF i)

instance Functor1 RejectF where
  fmap1 = fmap

instance NFData (RejectF i a) where
  rnf (RejectF a) = rnf a

deriving instance Ord (RejectF i a)

deriving instance Read (RejectF '[Reject] a)

deriving instance Show (RejectF i a)

instance ToXML (RejectF i a) where
  toXML (RejectF attrs) = [makeElement "Reject" () $ toAttrs attrs]

-- | See <https://www.twilio.com/docs/api/twiml/reject#attributes>.
data RejectAttributes = RejectAttributes
  { _rejectReason :: Maybe Reason
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default RejectAttributes where
  def = RejectAttributes
    { _rejectReason = def
    }

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

data Pause

data PauseF i a where
  PauseF :: PauseAttributes -> a -> PauseF '[Pause] a

deriving instance Data a => Data (PauseF '[Pause] a)

deriving instance Eq a => Eq (PauseF i a)

deriving instance Functor (PauseF i)

instance Functor1 PauseF where
  fmap1 = fmap

instance NFData a => NFData (PauseF i a) where
  rnf (PauseF a b) = rnf a `seq` rnf b

deriving instance Ord a => Ord (PauseF i a)

deriving instance Read a => Read (PauseF '[Pause] a)

deriving instance Show a => Show (PauseF i a)

instance ToXML a => ToXML (PauseF i a) where
  toXML (PauseF attrs a) = makeElement "Pause" () (toAttrs attrs) : toXML a

-- | See <https://www.twilio.com/docs/api/twiml/pause#attributes>.
data PauseAttributes = PauseAttributes
  { _pauseDuration :: Maybe Natural
  } deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance Default PauseAttributes where
  def = PauseAttributes
    { _pauseDuration = def
    }

instance ToAttrs PauseAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr "length" _pauseDuration
    ]

data End

data EndF i a where
  EndF :: EndF '[End] a

deriving instance Functor (EndF i)

instance Functor1 EndF where
  fmap1 = fmap

deriving instance Eq (EndF i a)

instance NFData (EndF i a) where
  rnf EndF = ()

deriving instance Ord (EndF i a)

deriving instance Read (EndF '[End] a)

deriving instance Show (EndF i a)

instance ToXML (EndF i a) where
  toXML EndF = []

data Voice'
data Messaging

data Twiml a where
  VoiceTwiml     :: forall i. VoiceTwiml'     i Void -> Twiml Voice'
  -- MessagingTwiml :: forall i. MessagingTwiml' i Void -> Twiml Messaging

instance ToElement (Twiml a) where
  toElement (VoiceTwiml twiml) = unode "Response" $ toXML twiml

instance Show (Twiml a) where
  -- show (VoiceTwiml     twiml) = "VoiceTwiml ("     ++ show twiml ++ ")"
  -- show (MessagingTwiml twiml) = "MessagingTwiml (" ++ show twiml ++ ")"
  show = showTwiml

showTwiml :: Twiml a -> String
showTwiml twiml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ ppElement (toElement twiml) ++ "\n"

instance Functor1 VoiceTwimlF where
  fmap1 = fmap

instance Show1 VoiceTwimlF where
  show1 = show

newtype VoiceTwimlF i a = VoiceTwimlF
  { getVoiceTwimlF ::
    (     SayF      i
      :+: PlayF     i
      :+: GatherF   i
      :+: SmsF      i -- Shared between Voice and Messaging TwiML
      :+: DialF     i
      :+: EnqueueF  i
      :+: LeaveF    i
      :+: HangupF   i
      :+: RecordF   i
      :+: RedirectF i -- Shared between Voice and Messaging TwiML
      :+: RejectF   i
      :+: PauseF    i
      :+: EndF      i -- Shared between Voice and Messaging TwiML
    ) a
  } deriving (Functor, Generic, Show, Typeable)

instance (f i :<: (SayF      i :+:
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
                   EndF      i    )) => f i :<: VoiceTwimlF i where
  inj = VoiceTwimlF . inj
  prj = prj . getVoiceTwimlF

instance ToXML a => ToXML (VoiceTwimlF i a) where
  toXML = toXML . getVoiceTwimlF

type VoiceTwiml = Twiml Voice'

type VoiceTwiml' i a = IxFree VoiceTwimlF i a

instance ToXML (VoiceTwiml' i Void) where
  toXML (IxFree f) = toXML f
  toXML _ = error "Impossible"

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
