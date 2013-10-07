{-#LANGUAGE EmptyDataDecls #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE NoMonomorphismRestriction #-}
{-#LANGUAGE Rank2Types #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE UndecidableInstances #-}

module Text.XML.Twiml
  ( Twiml
  -- * Types
  -- ** @\<Response\>@
  , Response
  , respond
  , toArrowXml
  -- ** URL
  , URL
  -- ** Method
  , Method(..)
  -- ** Key
  , Key(..)
  -- ** End
  , End
  , end
  -- * Primary Verbs
  -- ** @\<Say\>@
  , Say
  , SayAttributes(..)
  , Voice(..)
  , Lang(..)
  , LangAlice(..)
  , say
  , say'
  , sayMan
  , sayMan'
  , sayWoman
  , sayWoman'
  , sayAlice
  , sayAlice'
  , voice
  , sayAttributes
  -- ** @\<Play\>@
  , Play
  , PlayAttributes(..)
  , play
  , play'
  , playAttributes
  -- ** @\<Gather\>@
  , Gather
  , GatherAttributes(..)
  , gather
  , gather'
  , numDigits
  , gatherAttributes
  -- ** @\<Record\>@
  , Record
  , RecordAttributes(..)
  , record
  , record'
  , maxLength
  , transcribe
  , transcribeCallback
  , playBeep
  , recordAttributes
  -- ** @\<Sms\>@
  , Sms
  , SmsAttributes(..)
  , sms
  , sms'
  , Text.XML.Twiml.to
  , Text.XML.Twiml.from
  , statusCallback
  , smsAttributes
  -- ** @\<Dial\>@
  , Dial
  , DialAttributes(..)
  , DialNoun
  , dial
  , dial'
  , hangupOnStar
  , timeLimit
  , callerId
  , recordDial
  , dialAttributes
  -- * Secondary Verbs
  -- ** @\<Enqueue\>@
  , Enqueue
  , enqueue
  -- ** @\<Leave\>@
  , Leave
  , leave
  -- ** @\<Hangup\>@
  , Hangup
  , hangup
  -- ** @\<Redirect\>@
  , Redirect
  , RedirectAttributes(..)
  , redirect
  , redirect'
  , redirectAttributes
  -- ** @\<Reject\>@
  , Reject
  , RejectAttributes(..)
  , Reason(..)
  , reject
  , reject'
  , reason
  , rejectAttributes
  -- ** @\<Pause\>@
  , Pause
  , PauseAttributes(..)
  , pause
  , pause'
  , Text.XML.Twiml.length
  , pauseAttributes
  -- * Lenses
  , HasLoop
  , loop
  , HasAction
  , action
  , HasMethod
  , method
  , HasTimeout
  , timeout
  , HasFinishOnKey
  , finishOnKey
  ) where

import Control.Applicative (Applicative(..), (<$>))
-- import Control.Lens.Combinators ((&), (<&>))
import Control.Lens as L
import Data.Functor.Foldable (Base, Fix(..), Foldable(..))
import Data.Functor.Identity (Identity(..))
import Data.Natural (Natural)
import Text.XML.HXT.Core hiding (loop)

-- | The following allows us to place the constraint @(f a :/~ GatherNoun a)@ on
-- TwiML verbs that cannot be embedded in @\<Gather\>@. See
-- <http://stackoverflow.com/a/17794490>.

data Yes

data No

class TypeCast a b | a -> b

instance TypeCast a a

class TypeEq a b c | a b -> c

instance TypeEq x x Yes

instance TypeCast No b => TypeEq x y b

{- Twiml Types -}

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

data DialNoun = DialNoun

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

class TypeEq x GatherNoun No => NotGatherNoun x

instance TypeEq x GatherNoun No => NotGatherNoun x

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

sayAttributes :: Lens' (Say p) SayAttributes
sayAttributes = lens
  (\(Say (Twiml' (Fix (SayF attributes _ _)))) -> attributes)
  (\(Say (Twiml' (Fix (SayF _          n a))))    attributes ->
     Say (Twiml' (Fix (SayF attributes n a))))

setSayVoice :: SayAttributes -> Voice -> SayAttributes
setSayVoice attrs voice = attrs { sayVoice = Just voice }

setSayLoop :: SayAttributes -> Natural -> SayAttributes
setSayLoop attrs loop = attrs { sayLoop = Just loop }

voice :: Lens (Say p) (Say p) (Maybe Voice) Voice
voice = lens (^. sayAttributes . L.to sayVoice)
  (\t v -> over sayAttributes (flip setSayVoice v) t)

instance HasLoop (Say p) where
  getLoop = (^. sayAttributes . L.to sayLoop)
  setLoop t v = over sayAttributes (flip setSayLoop v) t

-- | See <https://www.twilio.com/docs/api/twiml/play#attributes>.
data PlayAttributes = PlayAttributes
  { playLoop :: Maybe Natural
  }

defaultPlayAttributes :: PlayAttributes
defaultPlayAttributes = PlayAttributes
  { playLoop = Nothing
  }

playAttributes :: Lens' (Play p) PlayAttributes
playAttributes = lens
  (\(Play (Twiml' (Fix (PlayF attributes _ _)))) -> attributes)
  (\(Play (Twiml' (Fix (PlayF _          n a))))    attributes ->
     Play (Twiml' (Fix (PlayF attributes n a))))

setPlayLoop :: PlayAttributes -> Natural -> PlayAttributes
setPlayLoop attrs loop = attrs { playLoop = Just loop }

instance HasLoop (Play p) where
  getLoop = (^. playAttributes . L.to playLoop)
  setLoop t v = over playAttributes (flip setPlayLoop v) t

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

instance HasAction (Gather p) where
  getAction = (^. gatherAttributes . L.to gatherAction)
  setAction t v = over gatherAttributes (flip setGatherAction v) t

setGatherMethod :: GatherAttributes -> Method -> GatherAttributes
setGatherMethod attrs method = attrs { gatherMethod = Just method }

instance HasMethod (Gather p) where
  getMethod = (^. gatherAttributes . L.to gatherMethod)
  setMethod t v = over gatherAttributes (flip setGatherMethod v) t

setGatherTimeout :: GatherAttributes -> Natural -> GatherAttributes
setGatherTimeout attrs timeout = attrs { gatherTimeout = Just timeout }

instance HasTimeout (Gather p) where
  getTimeout = (^. gatherAttributes . L.to gatherTimeout)
  setTimeout t v = over gatherAttributes (flip setGatherTimeout v) t

setGatherFinishOnKey :: GatherAttributes -> Key -> GatherAttributes
setGatherFinishOnKey attrs finishOnKey
  = attrs { gatherFinishOnKey = Just finishOnKey }

instance HasFinishOnKey (Gather p) where
  getFinishOnKey = (^. gatherAttributes . L.to gatherFinishOnKey)
  setFinishOnKey t v = over gatherAttributes (flip setGatherFinishOnKey v) t

setGatherNumDigits :: GatherAttributes -> Natural -> GatherAttributes
setGatherNumDigits attrs numDigits = attrs { gatherNumDigits = Just numDigits }

numDigits :: Lens (Gather p) (Gather p) (Maybe Natural) Natural
numDigits = lens (^. gatherAttributes . L.to gatherNumDigits)
  (\t v -> over gatherAttributes (flip setGatherNumDigits v) t)

gatherAttributes :: Lens' (Gather p) GatherAttributes
gatherAttributes = lens
  (\(Gather (Twiml' (Fix (GatherF attributes _ _)))) -> attributes)
  (\(Gather (Twiml' (Fix (GatherF _          n a))))    attributes ->
     Gather (Twiml' (Fix (GatherF attributes n a))))

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

instance HasAction (Record p) where
  getAction = (^. recordAttributes . L.to recordAction)
  setAction t v = over recordAttributes (flip setRecordAction v) t

setRecordMethod :: RecordAttributes -> Method -> RecordAttributes
setRecordMethod attrs method = attrs { recordMethod = Just method }

instance HasMethod (Record p) where
  getMethod = (^. recordAttributes . L.to recordMethod)
  setMethod t v = over recordAttributes (flip setRecordMethod v) t

setRecordTimeout :: RecordAttributes -> Natural -> RecordAttributes
setRecordTimeout attrs timeout = attrs { recordTimeout = Just timeout }

instance HasTimeout (Record p) where
  getTimeout = (^. recordAttributes . L.to recordTimeout)
  setTimeout t v = over recordAttributes (flip setRecordTimeout v) t

setRecordFinishOnKey :: RecordAttributes -> Key -> RecordAttributes
setRecordFinishOnKey attrs key = attrs { recordFinishOnKey = Just key }

instance HasFinishOnKey (Record p) where
  getFinishOnKey = (^. recordAttributes . L.to recordFinishOnKey)
  setFinishOnKey t v = over recordAttributes (flip setRecordFinishOnKey v) t

setRecordMaxLength :: RecordAttributes -> Natural -> RecordAttributes
setRecordMaxLength attrs length = attrs { recordMaxLength = Just length }

maxLength :: Lens (Record p) (Record p) (Maybe Natural) Natural
maxLength = lens (^. recordAttributes . L.to recordMaxLength)
  (\t v -> over recordAttributes (flip setRecordMaxLength v) t)

setRecordTranscribe :: RecordAttributes -> Bool -> RecordAttributes
setRecordTranscribe attrs transcribe
  = attrs { recordTranscribe = Just transcribe }

transcribe :: Lens (Record p) (Record p) (Maybe Bool) Bool
transcribe = lens (^. recordAttributes . L.to recordTranscribe)
  (\t v -> over recordAttributes (flip setRecordTranscribe v) t)

setRecordTranscribeCallback :: RecordAttributes -> URL -> RecordAttributes
setRecordTranscribeCallback attrs transcribeCallback
  = attrs { recordTranscribeCallback = Just transcribeCallback }

transcribeCallback :: Lens (Record p) (Record p) (Maybe URL) URL
transcribeCallback = lens (^. recordAttributes . L.to recordTranscribeCallback)
  (\t v -> over recordAttributes (flip setRecordTranscribeCallback v) t)

setRecordPlayBeep :: RecordAttributes -> Bool -> RecordAttributes
setRecordPlayBeep attrs playBeep = attrs { recordPlayBeep = Just playBeep }

playBeep :: Lens (Record p) (Record p) (Maybe Bool) Bool
playBeep = lens (^. recordAttributes . L.to recordPlayBeep)
  (\t v -> over recordAttributes (flip setRecordPlayBeep v) t)

recordAttributes :: Lens' (Record p) RecordAttributes
recordAttributes = lens
  (\(Record (Twiml' (Fix (RecordF attributes _ _)))) -> attributes)
  (\(Record (Twiml' (Fix (RecordF _          n a))))    attributes ->
     Record (Twiml' (Fix (RecordF attributes n a))))

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

to :: Lens (Sms p) (Sms p) (Maybe String) String
to = lens (^. smsAttributes . L.to smsTo)
  (\t v -> over smsAttributes (flip setSmsTo v) t)

setSmsFrom :: SmsAttributes -> String -> SmsAttributes
setSmsFrom attrs from = attrs { smsFrom = Just from }

from :: Lens (Sms p) (Sms p) (Maybe String) String
from = lens (^. smsAttributes . L.to smsFrom)
  (\t v -> over smsAttributes (flip setSmsFrom v) t)

setSmsAction :: SmsAttributes -> URL -> SmsAttributes
setSmsAction attrs action = attrs { smsAction = Just action }

instance HasAction (Sms p) where
  getAction = (^. smsAttributes . L.to smsAction)
  setAction t v = over smsAttributes (flip setSmsAction v) t

setSmsMethod :: SmsAttributes -> Method -> SmsAttributes
setSmsMethod attrs method = attrs { smsMethod = Just method }

instance HasMethod (Sms p) where
  getMethod = (^. smsAttributes . L.to smsMethod)
  setMethod t v = over smsAttributes (flip setSmsMethod v) t

setSmsStatusCallback :: SmsAttributes -> URL -> SmsAttributes
setSmsStatusCallback attrs statusCallback
  = attrs { smsStatusCallback = Just statusCallback }

statusCallback :: Lens (Sms p) (Sms p) (Maybe URL) URL
statusCallback = lens (^. smsAttributes . L.to smsStatusCallback)
  (\t v -> over smsAttributes (flip setSmsStatusCallback v) t)

smsAttributes :: Lens' (Sms p) SmsAttributes
smsAttributes = lens
  (\(Sms (Twiml' (Fix (SmsF attributes _ _)))) -> attributes)
  (\(Sms (Twiml' (Fix (SmsF _          n a))))    attributes ->
     Sms (Twiml' (Fix (SmsF attributes n a))))

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

instance HasAction (Dial p) where
  getAction = (^. dialAttributes . L.to dialAction)
  setAction t v = over dialAttributes (flip setDialAction v) t

setDialMethod :: DialAttributes -> Method -> DialAttributes
setDialMethod attrs method = attrs { dialMethod = Just method }

instance HasMethod (Dial p) where
  getMethod = (^. dialAttributes . L.to dialMethod)
  setMethod t v = over dialAttributes (flip setDialMethod v) t

setDialTimeout :: DialAttributes -> Natural -> DialAttributes
setDialTimeout attrs timeout = attrs { dialTimeout = Just timeout }

instance HasTimeout (Dial p) where
  getTimeout = (^. dialAttributes . L.to dialTimeout)
  setTimeout t v = over dialAttributes (flip setDialTimeout v) t

setDialHangupOnStar :: DialAttributes -> Bool -> DialAttributes
setDialHangupOnStar attrs hangupOnStar
  = attrs { dialHangupOnStar = Just hangupOnStar }

hangupOnStar :: Lens (Dial p) (Dial p) (Maybe Bool) Bool
hangupOnStar = lens (^. dialAttributes . L.to dialHangupOnStar)
  (\t v -> over dialAttributes (flip setDialHangupOnStar v) t)

setDialTimeLimit :: DialAttributes -> Natural -> DialAttributes
setDialTimeLimit attrs timeLimit = attrs { dialTimeLimit = Just timeLimit }

timeLimit :: Lens (Dial p) (Dial p) (Maybe Natural) Natural
timeLimit = lens (^. dialAttributes . L.to dialTimeLimit)
  (\t v -> over dialAttributes (flip setDialTimeLimit v) t)

setDialCallerId :: DialAttributes -> String -> DialAttributes
setDialCallerId attrs callerId = attrs { dialCallerId = Just callerId }

callerId :: Lens (Dial p) (Dial p) (Maybe String) String
callerId = lens (^. dialAttributes . L.to dialCallerId)
  (\t v -> over dialAttributes (flip setDialCallerId v) t)

setDialRecord :: DialAttributes -> Bool -> DialAttributes
setDialRecord attrs record = attrs { dialRecord = Just record }

recordDial :: Lens (Dial p) (Dial p) (Maybe Bool) Bool
recordDial = lens (^. dialAttributes . L.to dialRecord)
  (\t v -> over dialAttributes (flip setDialRecord v) t)

dialAttributes :: Lens' (Dial p) DialAttributes
dialAttributes = lens
  (\(Dial (Twiml' (Fix (DialF attributes _ _)))) -> attributes)
  (\(Dial (Twiml' (Fix (DialF _          n a))))    attributes ->
     Dial (Twiml' (Fix (DialF attributes n a))))

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

instance HasMethod (Redirect p) where
  getMethod = (^. redirectAttributes . L.to redirectMethod)
  setMethod t v = over redirectAttributes (flip setRedirectMethod v) t

redirectAttributes :: Lens' (Redirect p) RedirectAttributes
redirectAttributes = lens
  (\(Redirect (Twiml' (Fix (RedirectF attributes _)))) -> attributes)
  (\(Redirect (Twiml' (Fix (RedirectF _          n))))    attributes ->
     Redirect (Twiml' (Fix (RedirectF attributes n))))

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

reason :: Lens (Reject p) (Reject p) (Maybe Reason) Reason
reason = lens (^. rejectAttributes . L.to rejectReason)
  (\t v -> over rejectAttributes (flip setRejectReason v) t)

rejectAttributes :: Lens' (Reject p) RejectAttributes
rejectAttributes = lens
  (\(Reject (Twiml' (Fix (RejectF attributes)))) -> attributes)
  (\(Reject (Twiml' (Fix (RejectF _         ))))    attributes ->
     Reject (Twiml' (Fix (RejectF attributes))))

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

length :: Lens (Pause p) (Pause p) (Maybe Natural) Natural
length = lens (^. pauseAttributes . L.to pauseLength)
  (\t v -> over pauseAttributes (flip setPauseLength v) t)

pauseAttributes :: Lens' (Pause p) PauseAttributes
pauseAttributes = lens
  (\(Pause (Twiml' (Fix (PauseF attributes _)))) -> attributes)
  (\(Pause (Twiml' (Fix (PauseF _          a))))    attributes ->
     Pause (Twiml' (Fix (PauseF attributes a))))

{- Twiml -}

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
  GatherF   :: (Twiml GatherNoun t, NotGatherNoun p)
            => GatherAttributes
            -> t
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
            => String
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
  fmap _  EndF                   = EndF
  fmap f (SayF      attrs n   a) = SayF      attrs n   $ f a
  fmap f (PlayF     attrs url a) = PlayF     attrs url $ f a
  fmap f (GatherF   attrs n   a) = GatherF   attrs n   $ f a
  fmap f (RecordF   attrs url a) = RecordF   attrs url $ f a
  fmap f (SmsF      attrs n   a) = SmsF      attrs n   $ f a
  fmap f (DialF     attrs n   a) = DialF     attrs n   $ f a
  fmap f (EnqueueF        n   a) = EnqueueF        n   $ f a
  fmap _  LeaveF                 = LeaveF
  fmap _  HangupF                = HangupF
  fmap _ (RedirectF attrs url  ) = RedirectF attrs url
  fmap _ (RejectF   attrs      ) = RejectF   attrs
  fmap f (PauseF    attrs     a) = PauseF    attrs     $ f a

newtype Twiml' p = Twiml' { fromTwiml' :: Fix (TwimlF p) }

{- | TwiML is a set of instructions you can use to tell Twilio what to do
   when you receive an incoming call or SMS. See
   <https://www.twilio.com/docs/api/twiml>.

   This library provides a number of smart constructors for creating 'Twiml', as
   well as conversion functions for interop with other Haskell XML libraries,
   including HXT.

   As an example, the following Haskell code,

@
example
  = respond
  . sayMan \"Hello, world\"
  $ hangup
@

   Is transformed into,

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response\>
  \<Say voice=\"man\">Hello, world\</Say\>
  \<Hangup/\>
\</Response\>
@

-}
class Twiml p t | t -> p where
  toTwiml' :: t -> Twiml' p

type instance Base (Twiml' p) = TwimlF p

instance forall p. Foldable (Twiml' p) where
  project = fmap Twiml' . unFix . fromTwiml'
    where
      unFix (Fix f) = f

-- | 'End' is not a TwiML verb, but it is useful for terminating 'Twiml'.
newtype End p = End { fromEnd :: Twiml' p }

instance Twiml p (End p) where toTwiml' = fromEnd

end :: End p
end = End . Twiml' $ Fix EndF

-- | The @\<Say\>@ verb converts text to speech that is read back to the caller.
-- See <https://www.twilio.com/docs/api/twiml/say>.
newtype Say p = Say { fromSay :: Twiml' p }

instance Twiml p (Say p) where toTwiml' = fromSay

say :: Twiml p t => String -> t -> Say p
say = say' defaultSayAttributes

say' :: Twiml p t => SayAttributes -> String -> t -> Say p
say' attrs n = Say . Twiml' . Fix . SayF attrs n . fromTwiml' . toTwiml'

sayMan :: Twiml p t => String -> t -> Say p
sayMan = say' (defaultSayAttributes { sayVoice = Just $ Man Nothing })

sayMan' :: Twiml p t => Lang -> String -> t -> Say p
sayMan' lang
  = say' (defaultSayAttributes { sayVoice = Just . Man $ Just lang })

sayWoman :: Twiml p t => String -> t -> Say p
sayWoman = say' (defaultSayAttributes { sayVoice = Just $ Woman Nothing })

sayWoman' :: Twiml p t => Lang -> String -> t -> Say p
sayWoman' lang
  = say' (defaultSayAttributes { sayVoice = Just . Woman $ Just lang })

sayAlice :: Twiml p t => String -> t -> Say p
sayAlice = say' (defaultSayAttributes { sayVoice = Just $ Alice Nothing })

sayAlice' :: Twiml p t => LangAlice -> String -> t -> Say p
sayAlice' lang
  = say' (defaultSayAttributes { sayVoice = Just . Alice $ Just lang })

-- | The @\<Play\>@ verb plays an audio file back to the caller. Twilio
-- retrieves the file from a URL that you provide. See
-- <https://www.twilio.com/docs/api/twiml/play>.
newtype Play p = Play { fromPlay :: Twiml' p }

instance Twiml p (Play p) where toTwiml' = fromPlay

play :: Twiml p t => URL -> t -> Play p
play = play' defaultPlayAttributes

play' :: Twiml p t => PlayAttributes -> URL -> t -> Play p
play' attrs n = Play . Twiml' . Fix . PlayF attrs n . fromTwiml' . toTwiml'

-- | The @\<Gather\>@ verb collects digits that a caller enters into his or her
-- telephone keypad. See <https://www.twilio.com/docs/api/twiml/gather>.
--
-- You can nest @\<Say\>@, @\<Play\>@, and @\<Pause\>@ in @\<Gather\>@. See
-- <https://www.twilio.com/docs/api/twiml/gather#nesting-rules>.
--
-- Nesting rules are made explicit via an unexported functor, 'GatherNoun'.
newtype Gather p = Gather { fromGather :: Twiml' p }

instance Twiml p (Gather p) where toTwiml' = fromGather

gather :: (Twiml GatherNoun n, Twiml p t, NotGatherNoun p) => n -> t -> Gather p
gather = gather' defaultGatherAttributes

gather' :: (Twiml GatherNoun n, Twiml p t, NotGatherNoun p)
        => GatherAttributes -> n -> t -> Gather p
gather' attrs n
  = Gather . Twiml' . Fix . GatherF attrs n . fromTwiml' . toTwiml'

-- | The @\<Record\>@ verb records the caller's voice and returns to you the URL
-- of a file containing the audio recording. See
-- <https://www.twilio.com/docs/api/twiml/record>.
newtype Record p = Record { fromRecord :: Twiml' p }

instance Twiml p (Record p) where toTwiml' = fromRecord

record :: (Twiml p t, NotGatherNoun p) => URL -> t -> Record p
record = record' defaultRecordAttributes

record' :: (Twiml p t, NotGatherNoun p)
        => RecordAttributes -> URL -> t -> Record p
record' attrs url
  = Record . Twiml' . Fix . RecordF attrs url . fromTwiml' . toTwiml'

-- | The @\<Sms\>@ verb sends an SMS message to a phone number during a phone
-- call. See <https://www.twilio.com/docs/api/twiml/sms>.
newtype Sms p = Sms { fromSms :: Twiml' p }

instance Twiml p (Sms p) where toTwiml' = fromSms

sms :: (Twiml p t, NotGatherNoun p) => String -> t -> Sms p
sms = sms' defaultSmsAttributes

sms' :: (Twiml p t, NotGatherNoun p) => SmsAttributes -> String -> t -> Sms p
sms' attrs n = Sms . Twiml' . Fix . SmsF attrs n . fromTwiml' . toTwiml'

-- | The @\<Dial\>@ verb connects the current caller to another phone. See
-- <https://www.twilio.com/docs/api/twiml/dial>.
newtype Dial p = Dial { fromDial :: Twiml' p }

instance Twiml p (Dial p) where toTwiml' = fromDial

dial :: (Twiml p t, NotGatherNoun p) => Either DialNoun String -> t -> Dial p
dial = dial' defaultDialAttributes

dial' :: (Twiml p t, NotGatherNoun p)
      => DialAttributes -> Either DialNoun String -> t -> Dial p
dial' attrs n = Dial . Twiml' . Fix . DialF attrs n . fromTwiml' . toTwiml'

-- | The @\<Enqueue\>@ verb enqueues the current call in a call queue. See
-- <https://www.twilio.com/docs/api/twiml/enqueue>.
newtype Enqueue p = Enqueue { fromEnqueue :: Twiml' p }

instance Twiml p (Enqueue p) where toTwiml' = fromEnqueue

enqueue :: (Twiml p t, NotGatherNoun p) => String -> t -> Enqueue p
enqueue name = Enqueue . Twiml' . Fix . EnqueueF name . fromTwiml' . toTwiml'

-- | The @\<Leave\>@ verb transfers control of a call that is in a queue so that
-- the caller exits the queue and execution continues with the next verb after
-- the original @\<Enqueue\>@. See
-- <https://www.twilio.com/docs/api/twiml/leave>.
newtype Leave p = Leave { fromLeave :: Twiml' p }

instance Twiml p (Leave p) where toTwiml' = fromLeave

leave :: NotGatherNoun p => Leave p
leave = Leave . Twiml' $ Fix LeaveF

-- | The @\<Hangup\>@ verb ends a call. See
-- <https://www.twilio.com/docs/api/twiml/hangup>.
newtype Hangup p = Hangup { fromHangup :: Twiml' p }

instance Twiml p (Hangup p) where toTwiml' = fromHangup

hangup :: NotGatherNoun p => Hangup p
hangup = Hangup . Twiml' $ Fix HangupF

-- | The @\<Redirect\>@ verb transfers control of a call to the TwiML at a
-- different URL. See <https://www.twilio.com/docs/api/twiml/redirect>.
newtype Redirect p = Redirect { fromRedirect :: Twiml' p }

instance Twiml p (Redirect p) where toTwiml' = fromRedirect

redirect :: NotGatherNoun p => URL -> Redirect p
redirect = redirect' defaultRedirectAttributes

redirect' :: NotGatherNoun p => RedirectAttributes -> URL -> Redirect p
redirect' attrs url = Redirect . Twiml' . Fix $ RedirectF attrs url

-- | The @\<Reject\>@ verb rejects an incoming call to your Twilio number
-- without billing you. See
-- <https://www.twilio.com/docs/api/2010-04-01/twiml/reject>.
newtype Reject p = Reject { fromReject :: Twiml' p }

instance Twiml p (Reject p) where toTwiml' = fromReject

reject :: NotGatherNoun p => Reject p
reject = reject' defaultRejectAttributes

reject' :: NotGatherNoun p => RejectAttributes -> Reject p
reject' attrs = Reject . Twiml' . Fix $ RejectF attrs

-- | The @\<Pause\>@ verb waits silently for a specific number of seconds. See
-- <https://www.twilio.com/docs/api/twiml/pause>.
newtype Pause p = Pause { fromPause :: Twiml' p }

instance Twiml p (Pause p) where toTwiml' = fromPause

pause :: Twiml p t => t -> Pause p
pause = pause' defaultPauseAttributes

pause' :: Twiml p t => PauseAttributes -> t -> Pause p
pause' attrs = Pause . Twiml' . Fix . PauseF attrs . fromTwiml' . toTwiml'

{- Response -}

-- | The root element of Twilio's XML Markup is the @\<Response\>@ element. See
-- <https://www.twilio.com/docs/api/twiml/your_response#response-element>.
newtype Response = Response { fromResponse :: Twiml' Response }

instance Show Response where
  show t
    = unlines
    . flip runLA ()
    $ toArrowXml t >>> writeDocumentToString
      [ withXmlPi True
      , withOutputEncoding utf8
      , withIndent True
      ]

respond :: Twiml Response t => t -> Response
respond = Response . toTwiml'

toArrowXml :: ArrowXml a => Response -> a n XmlTree
toArrowXml
  = root [] . return . selem "Response" . cata toArrowXmls . fromResponse

toArrowXmls :: ArrowXml a => TwimlF p [a n XmlTree] -> [a n XmlTree]
toArrowXmls EndF
  = []
toArrowXmls (SayF attrs n a)
  = mkelem "Say"
      []
      [txt n]
  : a
toArrowXmls (PlayF attrs url a)
  = mkelem "Play"
      []
      [txt $ getURL url]
  : a
toArrowXmls (GatherF attrs n a)
  = mkelem "Gather"
      []
      (cata toArrowXmls $ toTwiml' n)
  : a
toArrowXmls (RecordF attrs url a)
  = mkelem "Record"
      []
      [txt $ getURL url]
  : a
toArrowXmls (SmsF attrs n a)
  = mkelem "Sms"
      []
      [txt n]
  : a
toArrowXmls (DialF attrs n a)
  = mkelem "Dial"
      []
      []
  : a
toArrowXmls (EnqueueF n a)
  = selem "Enqueue"
      [txt n]
  : a
toArrowXmls LeaveF
  = eelem "Leave"
  : []
toArrowXmls HangupF
  = eelem "Hangup"
  : []
toArrowXmls (RedirectF attrs url)
  = mkelem "Redirect"
      []
      [txt $ getURL url]
  : []
toArrowXmls (RejectF attrs)
  = aelem "Reject"
      []
  : []
toArrowXmls (PauseF attrs a)
  = aelem "Pause"
      []
  : a

{- Lenses -}

class HasLoop t where
  getLoop :: t -> Maybe Natural
  setLoop :: t -> Natural -> t

loop :: HasLoop t => Lens t t (Maybe Natural) Natural
loop = lens getLoop setLoop

class HasAction t where
  getAction :: t -> Maybe URL
  setAction :: t -> URL -> t

action :: HasAction t => Lens t t (Maybe URL) URL
action = lens getAction setAction

class HasMethod t where
  getMethod :: t -> Maybe Method
  setMethod :: t -> Method -> t

method :: HasMethod t => Lens t t (Maybe Method) Method
method = lens getMethod setMethod

class HasTimeout t where
  getTimeout :: t -> Maybe Natural
  setTimeout :: t -> Natural -> t

timeout :: HasTimeout t => Lens t t (Maybe Natural) Natural
timeout = lens getTimeout setTimeout

class HasFinishOnKey t where
  getFinishOnKey :: t -> Maybe Key
  setFinishOnKey :: t -> Key -> t

finishOnKey :: HasFinishOnKey t => Lens t t (Maybe Key) Key
finishOnKey = lens getFinishOnKey setFinishOnKey

{- Examples -}

example
  = respond
  . sayMan "Hello, world!"
  $ hangup

{-
example
  = respond
  . gather
    ( say
    $ end )
  . say
  $ end

example2
  = respond
  . (play <&> loop .~ 2)
  . say
  $ end
-}
