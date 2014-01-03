{-#LANGUAGE EmptyDataDecls #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE EmptyDataDecls #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE UndecidableInstances #-}

module Text.XML.Twiml.Types
  ( -- * Attributes
    -- ** @\<Say\>@
    SayAttributes(..)
  , defaultSayAttributes
    -- ** @\<Play\>@
  , PlayAttributes(..)
  , defaultPlayAttributes
    -- ** @\<Gather\>@
  , GatherAttributes(..)
  , defaultGatherAttributes
    -- ** @\<Record\>@
  , RecordAttributes(..)
  , defaultRecordAttributes
    -- ** @\<Sms\>@
  , SmsAttributes(..)
  , defaultSmsAttributes
    -- ** @\<Dial\>@
  , DialAttributes(..)
  , defaultDialAttributes
    -- ** @\<Enqueue\>@
  , EnqueueAttributes(..)
  , defaultEnqueueAttributes
    -- ** @\<Redirect\>@
  , RedirectAttributes(..)
  , defaultRedirectAttributes
    -- ** @\<Reject\>@
  , RejectAttributes(..)
  , defaultRejectAttributes
    -- ** @\<Pause\>@
  , PauseAttributes(..)
  , defaultPauseAttributes
    -- ** Lens Classes
  , HasLoop(..)
  , HasAction(..)
  , HasMethod(..)
  , HasTimeout(..)
  , HasFinishOnKey(..)
    -- * URL, Method & Transport
  , URL
  , parseURL
  , Method(..)
  , Transport(..)
    -- * Voices and Languages
  , Voice(..)
  , showVoice
  , showLang
  , Lang(..)
  , LangAlice(..)
    -- * Miscellaneous
  , Natural
  , Key(..)
  , PlayDigit(..)
  , ConferenceBeep(..)
  , Reason(..)
  , GatherNoun
  , NotGatherNoun
    -- * Basic Lens Functionality
  , Lens
  , Lens'
  , lens
  , (^.)
  , over
  , to'
    -- * Fix and Foldable
  , Fix(..)
  , Foldable(..)
  , Base(..)
    -- * Type Inequality
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

-- | See <https://www.twilio.com/docs/api/twiml/sip#transport>.
data Transport = TCP | UDP
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

{- Voices and Languages -}

-- | Voices supported by @\<Say\>@. See
-- <https://www.twilio.com/docs/api/twiml/say#attributes-voice>.
data Voice
  = Man   (Maybe Lang)
  | Woman (Maybe Lang)
  | Alice (Maybe LangAlice)

showVoice :: Voice -> String
showVoice (Man _) = "man"
showVoice (Woman _) = "woman"
showVoice (Alice _) = "alice"

showLang :: Voice -> Maybe String
showLang (Man lang) = fmap show lang 
showLang (Woman lang) = fmap show lang 
showLang (Alice lang) = fmap show lang 

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

-- This constraint lets us enforce TwiML nesting rules.

class TypeEq x GatherNoun No => NotGatherNoun x

instance TypeEq x GatherNoun No => NotGatherNoun x

{- Basic Lens Functionality -}

-- The following section extracts a number of definitions required to get
-- lenses, as defined in the lens package, working, without relying on the lens
-- package itself. If this turns out to be a bad idea, please email me.

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
  (#.) = \f -> \p -> p `seq` rmap f p

instance Profunctor (->) where
  dimap ab cd bc = cd . bc . ab
  {-# INLINE dimap #-}
  lmap = flip (.)
  {-# INLINE lmap #-}
  rmap = (.)
  {-# INLINE rmap #-}
  (#.) _ = unsafeCoerce
  {-# INLINE (#.) #-}

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
