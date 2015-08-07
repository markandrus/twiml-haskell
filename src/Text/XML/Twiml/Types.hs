{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Types
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Text.XML.Twiml.Types
  ( Digit(..)
  , Key(..)
  , Method(..)
  , Natural
  , URL
  , parseURL
  , Voice(..)
  , Lang(..)
  , LangAlice(..)
  , DialNoun(..)
    -- **** Number
  , NumberAttributes(..)
    -- **** Sip
  , SipAttributes(..)
  , Transport(..)
    -- **** Client
  , ClientAttributes(..)
    -- **** Conference 
  , ConferenceAttributes(..)
  , ConferenceBeep(..)
    -- **** Queue
  , QueueAttributes(..)
  , Reason(..)
  ) where

import Control.DeepSeq (NFData(..))
import Data.Data
import Data.Default
import GHC.Generics (Generic)
import Network.URI (URI(..), parseURIReference)

import Text.XML.Twiml.Internal

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

instance ToSomeNode URL where
  toSomeNode = toSomeNode . getURL

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

{- TwiML -}

{- Verbs -}

{- Say -}

{-
instance ToAttrs SayAttributes where
  toAttrs = flip makeAttrs
    [ makeAttr  "voice"      _sayVoice
    , makeAttr  "loop"       _sayLoop
    , makeAttr' "language"  (_sayVoice >=> lang) (either toAttrValue toAttrValue)
    ]
-}

instance ToAttrValue Voice where
  toAttrValue (Man   _) = "man"
  toAttrValue (Woman _) = "woman"
  toAttrValue (Alice _) = "alice"

{-
lang :: Voice -> Maybe (Either Lang LangAlice)
lang (Man   l) = Left  <$> l
lang (Woman l) = Left  <$> l
lang (Alice r) = Right <$> r
-}

{- Play -}

{- Gather -}

{- Dial -}

instance ToSomeNode (Either DialNoun String) where
  toSomeNode (Left  a) = SomeNode $ toElement a
  toSomeNode (Right a) = toSomeNode a
