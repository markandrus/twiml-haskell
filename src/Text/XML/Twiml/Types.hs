{-#LANGUAGE EmptyDataDecls #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE UndecidableInstances #-}

module Text.XML.Twiml.Types
  ( Natural
  , URL
  , parseURL
  , Method(..)
  , Key(..)
  , Digit(..)
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
  , Gather'
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
    -- ** Lens
    -- $lens
  , Lens
  , Lens'
  , lens
  , (^.)
  , over
  , to'
    -- ** Fix & Foldable
    -- $fix
  , Fix(..)
  , Text.XML.Twiml.Types.Foldable(..)
  , Base
    -- ** Type Inequality
    -- $type
  , (:/~)
  , Yes
  , No
  ) where

import Network.URI (URI(..), parseURIReference)
import Unsafe.Coerce (unsafeCoerce)

{- Attributes -}

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

-- | See <https://www.twilio.com/docs/api/twiml/play#attributes>.
data PlayAttributes = PlayAttributes
  { playLoop   :: Maybe Natural
  , playDigits :: Maybe [Digit]
  }

defaultPlayAttributes :: PlayAttributes
defaultPlayAttributes = PlayAttributes
  { playLoop   = Nothing
  , playDigits = Nothing
  }

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

-- | For some @Twiml p t@, the constraint @(p ':/~' 'Gather'')@ lets us enforce
-- TwiML nesting rules.
data Gather'

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

-- | See <https://www.twilio.com/docs/api/twiml/number#attributes>.
data NumberAttributes = NumberAttributes
  { numberSendDigits :: Maybe [Digit]
  , numberURL        :: Maybe URL
  , numberMethod     :: Maybe Method
  }

defaultNumberAttributes :: NumberAttributes
defaultNumberAttributes = NumberAttributes
  { numberSendDigits = Nothing
  , numberURL        = Nothing
  , numberMethod     = Nothing
  }

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

-- | See <https://www.twilio.com/docs/api/twiml/sip#transport>.
data Transport = TCP | UDP
  deriving Show

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

-- | See <https://www.twilio.com/docs/api/twiml/redirect#attributes>.
data RedirectAttributes = RedirectAttributes
  { redirectMethod :: Maybe Method
  }

defaultRedirectAttributes :: RedirectAttributes
defaultRedirectAttributes = RedirectAttributes
  { redirectMethod = Nothing
  }

-- | See <https://www.twilio.com/docs/api/twiml/reject#attributes>.
data RejectAttributes = RejectAttributes
  { rejectReason :: Maybe Reason
  }

defaultRejectAttributes :: RejectAttributes
defaultRejectAttributes = RejectAttributes
  { rejectReason = Nothing
  }

-- | The reason attribute takes the values \"rejected\" and \"busy.\" This tells
-- Twilio what message to play when rejecting a call. Selecting \"busy\" will
-- play a busy signal to the caller, while selecting \"rejected\" will play a
-- standard not-in-service response.
-- See <https://www.twilio.com/docs/api/twiml/reject#attributes-reason>.
data Reason = Rejected | Busy

instance Show Reason where
  show Rejected = "rejected"
  show Busy     = "busy"

-- | See <https://www.twilio.com/docs/api/twiml/pause#attributes>.
data PauseAttributes = PauseAttributes
  { pauseLength :: Maybe Natural
  }

defaultPauseAttributes :: PauseAttributes
defaultPauseAttributes = PauseAttributes
  { pauseLength = Nothing
  }

{- Attribute Lens Classes -}

class HasLoop t where
  loop :: Lens t t (Maybe Natural) Natural

class HasAction t where
  action :: Lens t t (Maybe URL) URL

class HasMethod t where
  method :: Lens t t (Maybe Method) Method

class HasTimeout t where
  timeout :: Lens t t (Maybe Natural) Natural

class HasFinishOnKey t where
  finishOnKey :: Lens t t (Maybe Key) Key

{- URL, Method & Transport -}

data URL = URL { getURL :: String }

instance Show URL where
  show = getURL

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
  deriving Show

type Natural = Int

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

instance Show Digit where
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

{- Basic Lens Functionality -}

-- $lens The following section extracts a number of definitions required to get
-- lenses, as defined in the lens package, working, without relying on the lens
-- package itself. Rather than use the following functions, consider installing
-- lens. See <https://hackage.haskell.org/package/lens>.

-- The following definitions were extracted from the lens package.

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = fmap (sbt s) $ afb (sa s)
{-# INLINE lens #-}

type Lens' s a = Lens s s a a

newtype Accessor r a = Accessor { runAccessor :: r }

instance Functor (Accessor r) where
  fmap _ (Accessor m) = Accessor m
  {-# INLINE fmap #-}

instance Contravariant (Accessor r) where
  contramap _ (Accessor m) = Accessor m
  {-# INLINE contramap #-}

type Getting r s a = (a -> Accessor r a) -> s -> Accessor r s

infixl 8 ^.

(^.) :: s -> Getting a s a -> a
s ^. l = runAccessor (l Accessor s)
{-# INLINE (^.) #-}

type Setting p s t a b = p a (Mutator b) -> s -> Mutator t

newtype Mutator a = Mutator { runMutator :: a }

instance Functor Mutator where
  fmap f (Mutator a) = Mutator $ f a
  {-# INLINE fmap #-}

over :: Profunctor p => Setting p s t a b -> p a b -> s -> t
over l f = runMutator #. l (Mutator #. f)

type IndexPreservingGetter s a
  = forall p f. (Profunctor p, Contravariant f, Functor f) => p a (f a) -> p s (f s)

to' :: (s -> a) -> IndexPreservingGetter s a
to' f = dimap f coerce
{-# INLINE to' #-}

coerce :: (Contravariant f, Functor f) => f a -> f b
coerce a = fmap absurd $ contramap absurd a

-- The following definition was extracted from the contravariant package.

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

-- The following definitions were extracted from the void package.

newtype Void = Void Void

absurd :: Void -> a
absurd (Void a) = absurd a

-- The following definitions were extracted from the profunctors package.

class Profunctor h where
  lmap :: (a -> b) -> h b c -> h a c
  rmap :: (b -> c) -> h a b -> h a c
  dimap :: (a -> b) -> (c -> d) -> h b c -> h a d
  dimap f g = lmap f . rmap g
  (#.) :: (b -> c) -> h a b -> h a c
  f #. p = p `seq` rmap f p


instance Profunctor (->) where
  dimap ab cd bc = cd . bc . ab
  {-# INLINE dimap #-}
  lmap = flip (.)
  {-# INLINE lmap #-}
  rmap = (.)
  {-# INLINE rmap #-}
  (#.) _ = unsafeCoerce
  {-# INLINE (#.) #-}

{- Fix & Foldable -}

-- $fix The following definitions were extracted from the recursion-schemes
-- package. See <https://hackage.haskell.org/package/recursion-schemes>.

newtype Fix f = Fix { unFix :: f (Fix f) }

type family Base t :: * -> *

class Functor (Base t) => Foldable t where
  project :: t -> Base t t
  cata :: (Base t a -> a) -> t -> a
  cata f = c where c = f . fmap c . project

{- Type Inequality -}

-- $type The following defines ':/~' to mean inequality at the type level.
-- Borrowed from HList. See <http://okmij.org/ftp/Haskell/typeEQ.html>.

data Yes

data No

class TypeCast a b | a -> b

instance TypeCast a a

class TypeEq a b c | a b -> c

instance TypeEq x x Yes

instance TypeCast No b => TypeEq x y b

class TypeEq x y No => (:/~) x y

instance TypeEq x y No => (:/~) x y
