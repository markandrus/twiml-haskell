{-#LANGUAGE EmptyDataDecls #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE Rank2Types #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE UndecidableInstances #-}

module Text.XML.Twiml.Internal
  ( Twiml(..)
  , Twiml'
  , Fix(..)
  , TwimlF(..)
    -- * Types
  , URL
  , Method
  , Key
  , PlayDigit
  , Reason
  , Voice(..)
  , Lang(..)
  , LangAlice(..)
  , Natural
  , DialNoun(..)
  , ConferenceBeep(..)
  , Transport(..)
  , GatherNoun
  , NotGatherNoun
  , Base
  , Foldable
    -- * Primary Verbs
    -- ** @\<Say\>@
  , SayAttributes(..)
  , defaultSayAttributes
  , setSayVoice
  , setSayLoop
    -- ** @\<Play\>@
  , PlayAttributes(..)
  , defaultPlayAttributes
  , setPlayLoop
  , setPlayDigits
    -- ** @\<Gather\>@
  , GatherAttributes(..)
  , defaultGatherAttributes
  , setGatherAction
  , setGatherMethod
  , setGatherTimeout
  , setGatherFinishOnKey
  , setGatherNumDigits
    -- ** @\<Record\>@
  , RecordAttributes(..)
  , defaultRecordAttributes
  , setRecordAction
  , setRecordMethod
  , setRecordTimeout
  , setRecordFinishOnKey
  , setRecordMaxLength
  , setRecordTranscribe
  , setRecordTranscribeCallback
  , setRecordPlayBeep
    -- ** @\<Sms\>@
  , SmsAttributes(..)
  , defaultSmsAttributes
  , setSmsTo
  , setSmsFrom
  , setSmsAction
  , setSmsMethod
  , setSmsStatusCallback
    -- ** @\<Dial\>@
  , DialAttributes(..)
  , defaultDialAttributes
  , setDialAction
  , setDialMethod
  , setDialTimeout
  , setDialHangupOnStar
  , setDialTimeLimit
  , setDialCallerId
  , setDialRecord
    -- *** @\<Number\>@
  , NumberAttributes(..)
  , defaultNumberAttributes
    -- *** @\<Sip\>@
  , SipAttributes(..)
  , defaultSipAttributes
    -- *** @\<Client\>@
  , ClientAttributes(..)
  , defaultClientAttributes
    -- *** @\<Conference\>@
  , ConferenceAttributes(..)
  , defaultConferenceAttributes
    -- *** @\<Queue\>@
  , QueueAttributes(..)
  , defaultQueueAttributes
    -- * Secondary Verbs
    -- ** @\<Enqueue\>@
  , EnqueueAttributes(..)
  , defaultEnqueueAttributes
    -- ** @\<Redirect\>@
  , RedirectAttributes(..)
  , defaultRedirectAttributes
  , setRedirectMethod
    -- ** @\<Reject\>@
  , RejectAttributes(..)
  , defaultRejectAttributes
  , setRejectReason
    -- ** @\<Pause\>@
  , PauseAttributes(..)
  , defaultPauseAttributes
  , setPauseLength
    -- * Lenses
  , Lens
  , lens
  ) where

type Natural = Int

{- Basic Lens Functionality -}

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = fmap (sbt s) $ afb (sa s)
{-# INLINE lens #-}

{- Fix and Foldable -}

-- $fixAndFoldable The following definitions were extracted from the recursion-schemes package.

newtype Fix f = Fix { unFix :: f (Fix f) }

type family Base t :: * -> *

class Functor (Base t) => Foldable t where
  project :: t -> Base t t
  cata :: (Base t a -> a) -> t -> a
  cata f = c where c = f . fmap c . project

{- Type Inequality -}

-- See <http://stackoverflow.com/a/17794490>.

data Yes

data No

class TypeCast a b | a -> b

instance TypeCast a a

class TypeEq a b c | a b -> c

instance TypeEq x x Yes

instance TypeCast No b => TypeEq x y b

{- TwimlF -}

-- | This is the 'Functor' we use when folding 'Twiml'.
data TwimlF p a where
  EndF      :: TwimlF p a
  SayF      :: SayAttributes
            -> String
            -> a
            -> TwimlF p a
  PlayF     :: PlayAttributes
            -> URL
            -> a
            -> TwimlF p a
  GatherF   :: NotGatherNoun p
            => GatherAttributes
            -> Twiml' GatherNoun
            -> a
            -> TwimlF p a
  RecordF   :: NotGatherNoun p
            => RecordAttributes
            -> URL
            -> a
            -> TwimlF p a
  SmsF      :: NotGatherNoun p
            => SmsAttributes
            -> String
            -> a
            -> TwimlF p a
  DialF     :: NotGatherNoun p
            => DialAttributes
            -> Either DialNoun String
            -> a
            -> TwimlF p a
  EnqueueF  :: NotGatherNoun p
            => EnqueueAttributes
            -> String
            -> a
            -> TwimlF p a
  LeaveF    :: NotGatherNoun p
            => TwimlF p a
  HangupF   :: NotGatherNoun p
            => TwimlF p a
  RedirectF :: NotGatherNoun p
            => RedirectAttributes
            -> URL
            -> TwimlF p a
  RejectF   :: NotGatherNoun p
            => RejectAttributes
            -> TwimlF p a
  PauseF    :: PauseAttributes
            -> a
            -> TwimlF p a

instance Functor (TwimlF p) where
  fmap f  EndF             = EndF
  fmap f (SayF      a b c) = SayF      a b $ f c
  fmap f (PlayF     a b c) = PlayF     a b $ f c
  fmap f (GatherF   a b c) = GatherF   a b $ f c
  fmap f (RecordF   a b c) = RecordF   a b $ f c
  fmap f (SmsF      a b c) = SmsF      a b $ f c
  fmap f (DialF     a b c) = DialF     a b $ f c
  fmap f (EnqueueF  a b c) = EnqueueF  a b $ f c
  fmap f  LeaveF           = LeaveF
  fmap f  HangupF          = HangupF
  fmap f (RedirectF a b)   = RedirectF a b
  fmap f (RejectF   a)     = RejectF   a
  fmap f (PauseF    a b)   = PauseF    a   $ f b

type instance Base (Fix (TwimlF p)) = TwimlF p

instance Foldable (Fix (TwimlF p)) where
  project = unFix

{- Twiml -}

type Twiml' p = Fix (TwimlF p)

class Twiml p t | t -> p where
  toTwiml' :: t -> Twiml' p

instance Twiml p (Twiml' p) where
  toTwiml' = id

{- Twiml Attributes -}

-- | See <https://www.twilio.com/docs/api/twiml/say#attributes>.
data SayAttributes = SayAttributes
  { sayVoice :: Maybe Voice
  , sayLoop  :: Maybe Natural
  }

defaultSayAttributes :: SayAttributes
defaultSayAttributes = SayAttributes
  { sayVoice = Nothing
  , sayLoop  = Nothing
  }

setSayVoice :: SayAttributes -> Voice -> SayAttributes
setSayVoice attrs voice = attrs { sayVoice = Just voice }

setSayLoop :: SayAttributes -> Natural -> SayAttributes
setSayLoop attrs loop = attrs { sayLoop = Just loop }

-- | See <https://www.twilio.com/docs/api/twiml/play#attributes>.
data PlayAttributes = PlayAttributes
  { playLoop :: Maybe Natural
  , playDigits' :: Maybe [PlayDigit]
  }

defaultPlayAttributes :: PlayAttributes
defaultPlayAttributes = PlayAttributes
  { playLoop    = Nothing
  , playDigits' = Nothing
  }

setPlayLoop :: PlayAttributes -> Natural -> PlayAttributes
setPlayLoop attrs loop = attrs { playLoop = Just loop }

setPlayDigits :: PlayAttributes -> [PlayDigit] -> PlayAttributes
setPlayDigits attrs digits = attrs { playDigits' = Just digits }

-- | See <https://www.twilio.com/docs/api/twiml/gather#attributes>.
data GatherAttributes = GatherAttributes
  { gatherAction      :: Maybe URL
  , gatherMethod      :: Maybe Method
  , gatherTimeout     :: Maybe Natural
  , gatherFinishOnKey :: Maybe Key
  , gatherNumDigits   :: Maybe Natural
  }

defaultGatherAttributes :: GatherAttributes
defaultGatherAttributes = GatherAttributes
  { gatherAction      = Nothing
  , gatherMethod      = Nothing
  , gatherTimeout     = Nothing
  , gatherFinishOnKey = Nothing
  , gatherNumDigits   = Nothing
  }

setGatherAction :: GatherAttributes -> URL -> GatherAttributes
setGatherAction attrs action = attrs { gatherAction = Just action }

setGatherMethod :: GatherAttributes -> Method -> GatherAttributes
setGatherMethod attrs method = attrs { gatherMethod = Just method }

setGatherTimeout :: GatherAttributes -> Natural -> GatherAttributes
setGatherTimeout attrs timeout = attrs { gatherTimeout = Just timeout }

setGatherFinishOnKey :: GatherAttributes -> Key -> GatherAttributes
setGatherFinishOnKey attrs finishOnKey
  = attrs { gatherFinishOnKey = Just finishOnKey }

setGatherNumDigits :: GatherAttributes -> Natural -> GatherAttributes
setGatherNumDigits attrs numDigits = attrs { gatherNumDigits = Just numDigits }

-- | See <https://www.twilio.com/docs/api/twiml/record#attributes>.
data RecordAttributes = RecordAttributes
  { recordAction             :: Maybe URL
  , recordMethod             :: Maybe Method
  , recordTimeout            :: Maybe Natural
  , recordFinishOnKey        :: Maybe Key
  , recordMaxLength          :: Maybe Natural
  , recordTranscribe         :: Maybe Bool
  , recordTranscribeCallback :: Maybe URL
  , recordPlayBeep           :: Maybe Bool
  }

defaultRecordAttributes :: RecordAttributes
defaultRecordAttributes = RecordAttributes
  { recordAction             = Nothing
  , recordMethod             = Nothing
  , recordTimeout            = Nothing
  , recordFinishOnKey        = Nothing
  , recordMaxLength          = Nothing
  , recordTranscribe         = Nothing
  , recordTranscribeCallback = Nothing
  , recordPlayBeep           = Nothing
  }

setRecordAction :: RecordAttributes -> URL -> RecordAttributes
setRecordAction attrs action = attrs { recordAction = Just action }

setRecordMethod :: RecordAttributes -> Method -> RecordAttributes
setRecordMethod attrs method = attrs { recordMethod = Just method }

setRecordTimeout :: RecordAttributes -> Natural -> RecordAttributes
setRecordTimeout attrs timeout = attrs { recordTimeout = Just timeout }

setRecordFinishOnKey :: RecordAttributes -> Key -> RecordAttributes
setRecordFinishOnKey attrs key = attrs { recordFinishOnKey = Just key }

setRecordMaxLength :: RecordAttributes -> Natural -> RecordAttributes
setRecordMaxLength attrs length = attrs { recordMaxLength = Just length }

setRecordTranscribe :: RecordAttributes -> Bool -> RecordAttributes
setRecordTranscribe attrs transcribe
  = attrs { recordTranscribe = Just transcribe }

setRecordTranscribeCallback :: RecordAttributes -> URL -> RecordAttributes
setRecordTranscribeCallback attrs transcribeCallback
  = attrs { recordTranscribeCallback = Just transcribeCallback }

setRecordPlayBeep :: RecordAttributes -> Bool -> RecordAttributes
setRecordPlayBeep attrs playBeep = attrs { recordPlayBeep = Just playBeep }

-- | See <https://www.twilio.com/docs/api/twiml/sms#attributes>.
data SmsAttributes = SmsAttributes
  { smsTo             :: Maybe String
  , smsFrom           :: Maybe String
  , smsAction         :: Maybe URL
  , smsMethod         :: Maybe Method
  , smsStatusCallback :: Maybe URL
  }

defaultSmsAttributes :: SmsAttributes
defaultSmsAttributes = SmsAttributes
  { smsTo             = Nothing
  , smsFrom           = Nothing
  , smsAction         = Nothing
  , smsMethod         = Nothing
  , smsStatusCallback = Nothing
  }

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

-- | See <https://www.twilio.com/docs/api/twiml/dial#attributes>.
data DialAttributes = DialAttributes
  { dialAction       :: Maybe URL
  , dialMethod       :: Maybe Method
  , dialTimeout      :: Maybe Natural
  , dialHangupOnStar :: Maybe Bool
  , dialTimeLimit    :: Maybe Natural
  , dialCallerId     :: Maybe String
  , dialRecord       :: Maybe Bool
  }

defaultDialAttributes :: DialAttributes
defaultDialAttributes = DialAttributes
  { dialAction       = Nothing
  , dialMethod       = Nothing
  , dialTimeout      = Nothing
  , dialHangupOnStar = Nothing
  , dialTimeLimit    = Nothing
  , dialCallerId     = Nothing
  , dialRecord       = Nothing
  }

setDialAction :: DialAttributes -> URL -> DialAttributes
setDialAction attrs action = attrs { dialAction = Just action }

setDialMethod :: DialAttributes -> Method -> DialAttributes
setDialMethod attrs method = attrs { dialMethod = Just method }

setDialTimeout :: DialAttributes -> Natural -> DialAttributes
setDialTimeout attrs timeout = attrs { dialTimeout = Just timeout }

setDialHangupOnStar :: DialAttributes -> Bool -> DialAttributes
setDialHangupOnStar attrs hangupOnStar
  = attrs { dialHangupOnStar = Just hangupOnStar }

setDialTimeLimit :: DialAttributes -> Natural -> DialAttributes
setDialTimeLimit attrs timeLimit = attrs { dialTimeLimit = Just timeLimit }

setDialCallerId :: DialAttributes -> String -> DialAttributes
setDialCallerId attrs callerId = attrs { dialCallerId = Just callerId }

setDialRecord :: DialAttributes -> Bool -> DialAttributes
setDialRecord attrs record = attrs { dialRecord = Just record }

-- | See <https://www.twilio.com/docs/api/twiml/redirect#attributes>.
data RedirectAttributes = RedirectAttributes
  { redirectMethod :: Maybe Method
  }

defaultRedirectAttributes :: RedirectAttributes
defaultRedirectAttributes = RedirectAttributes
  { redirectMethod = Nothing
  }

setRedirectMethod :: RedirectAttributes -> Method -> RedirectAttributes
setRedirectMethod attrs method = attrs { redirectMethod = Just method }

-- | See <https://www.twilio.com/docs/api/twiml/reject#attributes>.
data RejectAttributes = RejectAttributes
  { rejectReason :: Maybe Reason
  }

defaultRejectAttributes :: RejectAttributes
defaultRejectAttributes = RejectAttributes
  { rejectReason = Nothing
  }

setRejectReason :: RejectAttributes -> Reason -> RejectAttributes
setRejectReason attrs reason = attrs { rejectReason = Just reason }

-- | See <https://www.twilio.com/docs/api/twiml/pause#attributes>.
data PauseAttributes = PauseAttributes
  { pauseLength :: Maybe Natural
  }

defaultPauseAttributes :: PauseAttributes
defaultPauseAttributes = PauseAttributes
  { pauseLength = Nothing
  }

setPauseLength :: PauseAttributes -> Natural -> PauseAttributes
setPauseLength attrs length = attrs { pauseLength = Just length }

-- | See <https://www.twilio.com/docs/api/twiml/enqueue#attributes>.
data EnqueueAttributes = EnqueueAttributes
  { enqueueAction        :: Maybe URL
  , enqueueMethod        :: Maybe Method
  , enqueueWaitURL       :: Maybe URL
  , enqueueWaitURLMethod :: Maybe Method
  }

defaultEnqueueAttributes :: EnqueueAttributes
defaultEnqueueAttributes = EnqueueAttributes
  { enqueueAction        = Nothing
  , enqueueMethod        = Nothing
  , enqueueWaitURL       = Nothing
  , enqueueWaitURLMethod = Nothing
  }

{- Twiml Datatypes -}

data URL = URL { getURL :: String }

instance Show URL where
  show = getURL

data Method = GET | POST
  deriving Show

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

instance Show Key where
  show K0     = "0"
  show K1     = "1"
  show K2     = "2"
  show K3     = "3"
  show K4     = "4"
  show K5     = "5"
  show K6     = "6"
  show K7     = "7"
  show K8     = "8"
  show K9     = "9"
  show KStar  = "*"
  show KPound = "#"

-- | Voices supported by @\<Say\>@. See
-- <https://www.twilio.com/docs/api/twiml/say#attributes-voice>.
data Voice
  = Man   (Maybe Lang)
  | Woman (Maybe Lang)
  | Alice (Maybe LangAlice)

-- | Languages spoken by voices 'Man' and 'Woman'. See
-- <https://www.twilio.com/docs/api/twiml/say#attributes-manwoman>.
data Lang
  = English
  | EnglishUK
  | Spanish
  | French
  | German
  | Italian

instance Show Lang where
  show English   = "en"
  show EnglishUK = "en-gb"
  show Spanish   = "es"
  show French    = "fr"
  show German    = "de"
  show Italian   = "it"

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

instance Show LangAlice where
  show DaDK = "da-DK"
  show DeDE = "de-DE"
  show EnAU = "en-AU"
  show EnCA = "en-CA"
  show EnGB = "en-GB"
  show EnIN = "en-IN"
  show EnUS = "en-US"
  show CaES = "ca-ES"
  show EsES = "es-ES"
  show EsMX = "es-MX"
  show FiFI = "fi-FI"
  show FrCA = "fr-CA"
  show FrFR = "fr-FR"
  show ItIT = "it-IT"
  show JaJP = "ja-JP"
  show KoKR = "ko-KR"
  show NbNO = "nb-NO"
  show NlNL = "nl-NL"
  show PlPL = "pl-PL"
  show PtBR = "pt-BR"
  show PtPT = "pt-PT"
  show RuRU = "ru-RU"
  show SvSE = "sv-SE"
  show ZhCN = "zh-CN"
  show ZhHK = "zh-HK"
  show ZhTW = "zh-TW"

-- FIXME: Rename `PlayKey`.

-- | See <https://www.twilio.com/docs/api/twiml/number#attributes>.
data NumberAttributes = NumberAttributes
  { numberSendDigits :: Maybe [PlayDigit]
  , numberURL        :: Maybe URL
  , numberMethod     :: Maybe Method
  }

defaultNumberAttributes :: NumberAttributes
defaultNumberAttributes = NumberAttributes
  { numberSendDigits = Nothing
  , numberURL        = Nothing
  , numberMethod     = Nothing
  }

-- | See <https://www.twilio.com/docs/api/twiml/sip#transport>.
data Transport = TCP | UDP
  deriving Show

-- | See <https://www.twilio.com/docs/api/twiml/sip#attributes>.
data SipAttributes = SipAttributes
  { sipUsername  :: Maybe String
  , sipPassword  :: Maybe String
  , sipTransport :: Maybe Transport
  , sipHeaders   :: Maybe String    -- NOTE: Under 1024 characters.
  , sipURL       :: Maybe URL
  , sipMethod    :: Maybe Method
  }

defaultSipAttributes :: SipAttributes
defaultSipAttributes = SipAttributes
  { sipUsername  = Nothing
  , sipPassword  = Nothing
  , sipTransport = Nothing
  , sipHeaders   = Nothing
  , sipURL       = Nothing
  , sipMethod    = Nothing
  }

-- | See <https://www.twilio.com/docs/api/twiml/client#attributes>.
data ClientAttributes = ClientAttributes
  { clientURL    :: Maybe URL
  , clientMethod :: Maybe Method
  }

defaultClientAttributes :: ClientAttributes
defaultClientAttributes = ClientAttributes
  { clientURL    = Nothing
  , clientMethod = Nothing
  }

-- | See <https://www.twilio.com/docs/api/twiml/conference#attributes-beep>.
data ConferenceBeep
  = Yes
  | No
  | OnExit
  | OnEnter

instance Show ConferenceBeep where
  show Yes     = "yes"
  show No      = "no"
  show OnExit  = "onExit"
  show OnEnter = "onEnter"

-- | See <https://www.twilio.com/docs/api/twiml/conference#attributes>.
data ConferenceAttributes = ConferenceAttributes
  { conferenceMuted           :: Maybe Bool
  , conferenceBeep            :: Maybe Bool
  , conferenceStartOnEnter    :: Maybe Bool
  , conferenceEndOnExit       :: Maybe Bool
  , conferenceWaitURL         :: Maybe URL
  , conferenceWaitMethod      :: Maybe Method
  , conferenceMaxParticipants :: Maybe Natural -- FIXME: Non-zero, less than 40.
  }

defaultConferenceAttributes :: ConferenceAttributes
defaultConferenceAttributes = ConferenceAttributes
  { conferenceMuted           = Nothing
  , conferenceBeep            = Nothing
  , conferenceStartOnEnter    = Nothing
  , conferenceEndOnExit       = Nothing
  , conferenceWaitURL         = Nothing
  , conferenceWaitMethod      = Nothing
  , conferenceMaxParticipants = Nothing
  }

-- | See <https://www.twilio.com/docs/api/twiml/queue#attributes>.
data QueueAttributes = QueueAttributes
  { queueURL    :: Maybe URL
  , queueMethod :: Maybe Method
  }

defaultQueueAttributes :: QueueAttributes
defaultQueueAttributes = QueueAttributes
  { queueURL    = Nothing
  , queueMethod = Nothing
  }

-- | See <https://www.twilio.com/docs/api/twiml/dial#nouns>.
data DialNoun
  = Number     NumberAttributes     String
  | Sip        SipAttributes        URL    -- NOTE: URL must be under 255 characters.
  | Client     ClientAttributes     String
  | Conference ConferenceAttributes String
  | Queue      QueueAttributes      String

-- | The reason attribute takes the values \"rejected\" and \"busy.\" This tells
-- Twilio what message to play when rejecting a call. Selecting \"busy\" will
-- play a busy signal to the caller, while selecting \"rejected\" will play a
-- standard not-in-service response.
-- See <https://www.twilio.com/docs/api/twiml/reject#attributes-reason>
data Reason = Rejected | Busy

instance Show Reason where
  show Rejected = "rejected"
  show Busy     = "busy"

data GatherNoun

-- This constraint lets us enforce TwiML nesting rules.

class TypeEq x GatherNoun No => NotGatherNoun x

instance TypeEq x GatherNoun No => NotGatherNoun x

-- | The ‘digits’ attribute lets you play DTMF tones during a call. See
-- <https://www.twilio.com/docs/api/twiml/play#attributes-digits>.
data PlayDigit
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

instance Show PlayDigit where
  show D0 = "0"
  show D1 = "1"
  show D2 = "2"
  show D3 = "3"
  show D4 = "4"
  show D5 = "5"
  show D6 = "6"
  show D7 = "7"
  show D8 = "8"
  show D9 = "9"
  show W  = "w"
