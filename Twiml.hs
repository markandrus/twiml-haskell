{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies,
  MultiParamTypeClasses, NoMonomorphismRestriction, Rank2Types, TypeFamilies,
  TypeOperators, UndecidableInstances #-}

module Twiml
  ( Twiml
  , Null
  , Key
  , Response
  , respond
  , toArrowXml
  -- * Primary Verbs
  -- ** @\<Say\>@
  , Say
  , SayAttributes(..)
  , Voice(..)
  , Lang(..)
  , LangAlice(..)
  , sayMan
  , sayMan'
  , sayWoman
  , sayWoman'
  , sayAlice
  , sayAlice'
  -- ** @\<Play\>@
  , Play
  , PlayAttributes(..)
  , play
  , play'
  -- ** @\<Gather\>@
  , Gather
  , GatherAttributes(..)
  , gatherNull
  , gather
  -- ** @\<Record\>@
  , Record
  , RecordAttributes(..)
  , record
  -- ** @\<Sms\>@
  , Sms
  , SmsAttributes(..)
  , sms
  -- ** @\<Dial\>@
  , Dial
  , DialAttributes(..)
  , DialNoun
  , dial
  -- * Secondary Verbs
  -- ** @\<Enqueue\>@
  , Enqueue
  , EnqueueAttributes(..)
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
  -- ** @\<Reject\>@
  , Reject
  , RejectAttributes(..)
  , Reason
  , reject
  , reject'
  -- ** @\<Pause\>@
  , Pause
  , PauseAttributes(..)
  , pause
  , pause'
  ) where

import Control.Arrow ((>>>))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Foldable (Base, Fix(..), Foldable(..))
import Data.Maybe (catMaybes)
import Data.Natural (Natural)
import Text.XML.HXT.Core

data URL = URL { getURL :: String }

instance Show URL where
  show = getURL

data Method = GET | POST
  deriving Show

type instance Base Twiml' = TwimlF

-- The following allows us to place the constraint @(f a :/~ GatherNoun a)@ on
-- TwiML verbs that cannot be embedded in @\<Gather\>@. See
-- <http://stackoverflow.com/a/17794490>.

data Yes

data No

class TypeCast a b | a -> b

instance TypeCast a a

class TypeEq a b c | a b -> c

instance TypeEq x x Yes

instance TypeCast No b => TypeEq x y b

class TypeEq x y No => (:/~) x y

instance TypeEq x y No => (:/~) x y

{- | TwiML is a set of instructions you can use to tell Twilio what to do
   when you receive an incoming call or SMS. See
   <https://www.twilio.com/docs/api/twiml>.

   This library provides a number of smart constructors for creating 'Twiml', as
   well as conversion functions for interop with other Haskell XML libraries,
   including HXT.

   As an example, the following Haskell code,

@
example
  = response
  . sayMan \"Hello, world\"
  . sayWoman \"Goodbye, world\"
  $ hangup
@

   Is transformed into,

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response\>
  \<Say voice=\"man\">Hello, world\</Say\>
  \<Say voice=\"woman\">Goodbye, world\</Say\>
  \<Hangup/\>
\</Response\>
@

-}

data Key
  = K0      -- ^ @0@
  | K1      -- ^ @1@
  | K2      -- ^ @2@
  | K3      -- ^ @3@
  | K4      -- ^ @4@
  | K5      -- ^ @5@
  | K6      -- ^ @6@
  | K7      -- ^ @7@
  | K8      -- ^ @8@
  | K9      -- ^ @9@
  | KStar   -- ^ @*@
  | KPound  -- ^ @#@

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

class Twiml t where
  -- toTwiml' :: t -> Twiml' ()
  toTwiml' :: t -> Twiml'

-- TODO: Document the options.

-- | See <https://www.twilio.com/docs/api/twiml/say#attributes>.
data SayAttributes = SayAttributes
  { sayVoice   :: Maybe Voice
  , sayLoop    :: Maybe Natural
  }

-- | See <https://www.twilio.com/docs/api/twiml/play#attributes>.
data PlayAttributes = PlayAttributes
  { playLoop :: Maybe Natural
  , playURL  :: URL
  }

-- | See <https://www.twilio.com/docs/api/twiml/gather#attributes>.
data GatherAttributes = GatherAttributes
  { gatherAction      :: Maybe URL
  , gatherMethod      :: Maybe Method
  , gatherTimeout     :: Maybe Natural
  , gatherFinishOnKey :: Maybe Key
  , gatherNumDigits   :: Maybe Natural
  -- , gatherNouns       :: Twiml' ()
  , gatherNouns       :: Twiml'
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

-- | See <https://www.twilio.com/docs/api/twiml/sms#attributes>.
data SmsAttributes = SmsAttributes
  { smsTo             :: Maybe String
  , smsFrom           :: Maybe String
  , smsAction         :: Maybe URL
  , smsMethod         :: Maybe Method
  , smsStatusCallback :: Maybe URL
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
  , dialNoun         :: Either DialNoun String
  }

-- | See <https://www.twilio.com/docs/api/twiml/enqueue#attributes>.
data EnqueueAttributes = EnqueueAttributes
  { enqueueName :: String
  }

-- | See <https://www.twilio.com/docs/api/twiml/redirect#attributes>.
data RedirectAttributes = RedirectAttributes
  { redirectMethod :: Maybe Method
  , redirectURL    :: URL
  }

-- | See <https://www.twilio.com/docs/api/twiml/reject#attributes>.
data RejectAttributes = RejectAttributes
  { rejectReason :: Maybe Reason }

-- | See <https://www.twilio.com/docs/api/twiml/pause#attributes>.
data PauseAttributes = PauseAttributes
  { pauseLength :: Maybe Natural
  }

data TwimlF a
  = NullF
  | SayF      SayAttributes                      String  a
  | PlayF     PlayAttributes                             a
  | GatherF   GatherAttributes                           a
  | RecordF   RecordAttributes                           a
  | SmsF      SmsAttributes                      String  a
  | DialF     DialAttributes    (Either DialNoun String) a
  | EnqueueF  EnqueueAttributes                          a
  | LeaveF
  | HangupF
  | RedirectF RedirectAttributes
  | RejectF   RejectAttributes
  | PauseF    PauseAttributes                            a
  deriving Functor

newtype Twiml' = Twiml' { fromTwiml' :: Fix TwimlF }

instance Foldable Twiml' where
  project (Twiml' (Fix NullF))
    = NullF
  project (Twiml' (Fix (SayF options noun next)))
    = SayF options noun $ Twiml' next
  project (Twiml' (Fix (PlayF options next)))
    = PlayF options $ Twiml' next
  project (Twiml' (Fix (GatherF options next)))
    = GatherF options $ Twiml' next
  project (Twiml' (Fix (RecordF options next)))
    = RecordF options $ Twiml' next
  project (Twiml' (Fix (SmsF options noun next)))
    = SmsF options noun $ Twiml' next
  project (Twiml' (Fix (DialF options noun next)))
    = DialF options noun $ Twiml' next
  project (Twiml' (Fix (EnqueueF options next)))
    = EnqueueF options $ Twiml' next
  project (Twiml' (Fix LeaveF))
    = LeaveF
  project (Twiml' (Fix HangupF))
    = HangupF
  project (Twiml' (Fix (RedirectF options)))
    = RedirectF options
  project (Twiml' (Fix (RejectF options)))
    = RejectF   options
  project (Twiml' (Fix (PauseF options next)))
    = PauseF options $ Twiml' next

toArrowXmls :: ArrowXml a => TwimlF [a n XmlTree] -> [a n XmlTree]
toArrowXmls (SayF options noun arrows)
  = mkelem "Say" [] []
  : arrows
toArrowXmls (PlayF options arrows)
  = selem "Play" []
  : arrows
toArrowXmls (GatherF options arrows)
  = mkelem "Gather" [] []
  : arrows
toArrowXmls (RecordF options arrows)
  = selem "Record" []
  : arrows
toArrowXmls (SmsF options noun arrows)
  = mkelem "Sms" [] []
  : arrows
toArrowXmls (DialF options noun arrows)
  = mkelem "Dial" [] []
  : arrows
toArrowXmls (EnqueueF options arrows)
  = selem "Enqueue" []
  : arrows
toArrowXmls (LeaveF)
  = [ eelem "Leave" ]
toArrowXmls (HangupF)
  = [ eelem "Hangup" ]
toArrowXmls (RedirectF options)
  = [ selem "Redirect" []
    ]
toArrowXmls (RejectF options)
  = [ selem "Reject" []
    ]
toArrowXmls (PauseF options arrows)
  = selem "Pause" []
  : arrows

-- | The root element of Twilio's XML Markup is the @\<Response\>@ element. In
-- any TwiML response to a Twilio request, all verb elements must be nested
-- within this element. Any other structure is considered invalid. See
-- <https://www.twilio.com/docs/api/twiml/your_response#response-element>.
newtype Response = Response { fromResponse :: Twiml' }

instance Show Response where
  show t
    = unlines
    . flip runLA ()
    $ toArrowXml t >>> writeDocumentToString
      [ withXmlPi True
      , withOutputEncoding utf8
      , withIndent True
      ]

respond :: Twiml t => (forall a. f a -> a) -> f t -> Response
respond f = Response . toTwiml' . f

-- | Convert a 'Response' into an @ArrowXml@ to be used with HXT. See
-- <http://hackage.haskell.org/package/hxt>.
toArrowXml :: ArrowXml a => Response -> a n XmlTree
toArrowXml = root [] . return . selem "Response" . cata toArrowXmls . fromResponse
{-
toArrowXml = root [] . return . selem "Response" . go . fromResponse where
  -- FIXME: We're not printing the correct TwiML for each tag!
  go NullF = []
  go (SayF t next) =
    mkelem "Say"
      ( catMaybes
        [ fmap (sattr "voice" . fst . voiceToStrings) $ sayVoice t
        , sayVoice t >>= fmap (sattr "language") . snd . voiceToStrings
        , fmap (sattr "loop" . show) $ sayLoop t
        ]
      ) [ txt $ sayMessage t ] : go next
  go (PlayF t next) =
    mkelem "Play"
      ( catMaybes
        [ fmap (sattr "loop" . show) $ playLoop t ]
      ) [ txt . getURL $ playURL t ] : go next
  go (GatherF t next) =
    mkelem "Gather" [] (go $ gatherNouns t) : go next
  go (RecordF t next) =
    mkelem "Record" [] [] : go next
  go (SmsF t noun next) =
    mkelem "Sms"
      ( catMaybes
        [ fmap (sattr "to"             . show  ) $ smsTo             t
        , fmap (sattr "from"           . show  ) $ smsFrom           t
        , fmap (sattr "action"         . getURL) $ smsAction         t
        , fmap (sattr "method"         . show  ) $ smsMethod         t
        , fmap (sattr "statusCallback" . getURL) $ smsStatusCallback t
        ]
      ) [ txt noun ] : go next
  go HangupF =
    [ eelem "Hangup" ]
  go (RedirectF t) =
    [ selem "Redirect"
      ( catMaybes
        [ fmap  (sattr "method" . show)  $ redirectMethod t
        , Just . sattr "url"    . getURL $ redirectURL    t ]
      )
    ]
  go (RejectF t) =
    [ selem "Reject"
      ( catMaybes
        [ fmap (sattr "reason" . show) $ rejectReason t ]
      )
    ]
  go (PauseF t next) =
    selem "Pause"
      ( catMaybes
        [ fmap (sattr "length" . show) $ pauseLength t ]
      ) : go next
-}

-- | Useful for terminating 'Twiml'.
newtype Null = Null { fromNull :: Twiml' }

null :: Identity Null
null = Identity . Null . Twiml' $ Fix NullF

instance Twiml Null where
  toTwiml' _ = Twiml' $ Fix NullF

-- | Voices supported by @\<Say\>@. See
-- <https://www.twilio.com/docs/api/twiml/say#attributes-voice>.
data Voice
  = Man   (Maybe Lang)
  | Woman (Maybe Lang)
  | Alice (Maybe LangAlice)

voiceToStrings :: Voice -> (String, Maybe String)
voiceToStrings (Man   lang) = ("man",   fmap show lang)
voiceToStrings (Woman lang) = ("woman", fmap show lang)
voiceToStrings (Alice lang) = ("alice", fmap show lang)

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

-- | The @\<Say\>@ verb converts text to speech that is read back to the caller.
-- See <https://www.twilio.com/docs/api/twiml/say>.
newtype Say = Say { fromSay :: Twiml' }

instance Twiml Say where
  toTwiml' = fromSay

sayMan :: (Functor f, Twiml t) => String -> f t -> f Say
sayMan noun
  = fmap (Say . Twiml' . Fix . SayF (SayAttributes (Just $ Man Nothing) Nothing) noun . fromTwiml' . toTwiml')

sayMan' :: (Functor f, Twiml t) => Lang -> String -> f t -> f Say
sayMan' lang noun
  = fmap (Say . Twiml' . Fix . SayF (SayAttributes (Just . Man $ Just lang) Nothing) noun . fromTwiml' . toTwiml')

sayWoman :: (Functor f, Twiml t) => String -> f t -> f Say
sayWoman noun
  = fmap (Say . Twiml' . Fix . SayF (SayAttributes (Just $ Woman Nothing) Nothing) noun . fromTwiml' . toTwiml')

sayWoman' :: (Functor f, Twiml t) => Lang -> String -> f t -> f Say
sayWoman' lang noun
  = fmap (Say . Twiml' . Fix . SayF (SayAttributes (Just . Woman $ Just lang) Nothing) noun . fromTwiml' . toTwiml')

sayAlice :: (Functor f, Twiml t) => String -> f t -> f Say
sayAlice noun
  = fmap (Say . Twiml' . Fix . SayF (SayAttributes (Just $ Alice Nothing) Nothing) noun . fromTwiml' . toTwiml')

sayAlice' :: (Functor f, Twiml t) => LangAlice -> String -> f t -> f Say
sayAlice' lang noun
  = fmap (Say . Twiml' . Fix . SayF (SayAttributes (Just . Alice $ Just lang) Nothing) noun . fromTwiml' . toTwiml')

-- | The @\<Play\>@ verb plays an audio file back to the caller. Twilio
-- retrieves the file from a URL that you provide. See
-- <https://www.twilio.com/docs/api/twiml/play>.
newtype Play = Play { fromPlay :: Twiml' }

instance Twiml Play where
  toTwiml' = fromPlay

play :: (Functor f, Twiml t) => URL -> f t -> f Play
play url = fmap (Play . Twiml' . Fix . PlayF (PlayAttributes Nothing url) . fromTwiml' . toTwiml')

play' :: (Functor f, Twiml t) => Natural -> URL -> f t -> f Play
play' i url = fmap (Play . Twiml' . Fix . PlayF (PlayAttributes (Just i) url) . fromTwiml' . toTwiml')

-- | The @\<Gather\>@ verb collects digits that a caller enters into his or her
-- telephone keypad. See <https://www.twilio.com/docs/api/twiml/gather>.
--
-- You can nest @\<Say\>@, @\<Play\>@, and @\<Pause\>@ in @\<Gather\>@. See
-- <https://www.twilio.com/docs/api/twiml/gather#nesting-rules>.
--
-- Nesting rules are made explicit via an unexported functor, 'GatherNoun'.
newtype Gather = Gather { fromGather :: Twiml' }

instance Twiml Gather where
  toTwiml' = fromGather

newtype GatherNoun t = GatherNoun { fromGatherNoun :: t }

instance Functor GatherNoun where
  fmap f = GatherNoun . f . fromGatherNoun

gather :: (Functor f, f a :/~ GatherNoun a, Twiml t) => GatherAttributes -> f t -> f Gather
gather options = fmap (Gather . Twiml' . Fix . GatherF options . fromTwiml' . toTwiml')

gatherNull :: GatherNoun Null
gatherNull = GatherNoun . Null . Twiml' $ Fix NullF

-- | The @\<Record\>@ verb records the caller's voice and returns to you the URL
-- of a file containing the audio recording. See
-- <https://www.twilio.com/docs/api/twiml/record>.
newtype Record = Record { fromRecord :: Twiml' }

instance Twiml Record where
  toTwiml' = fromRecord

record :: (Functor f, f a :/~ GatherNoun a, Twiml t) => RecordAttributes -> f t -> f Record
record options = fmap (Record . Twiml' . Fix . RecordF options . fromTwiml' . toTwiml')

-- | The @\<Sms\>@ verb sends an SMS message to a phone number during a phone
-- call. See <https://www.twilio.com/docs/api/twiml/sms>.
newtype Sms = Sms { fromSms :: Twiml' }

instance Twiml Sms where
  toTwiml' = fromSms

sms :: (Functor f, f a :/~ GatherNoun a, Twiml t)
    => SmsAttributes
    -> String
    -> f t
    -> Maybe (f Sms)
sms options noun
  | length noun < 160 = Just . fmap (Sms . Twiml' . Fix . SmsF options noun . fromTwiml' . toTwiml')
  | otherwise         = const Nothing

-- FIXME: Unimplemented!
data DialNoun = DialNoun

-- | The @\<Dial\>@ verb connects the current caller to another phone. See
-- <https://www.twilio.com/docs/api/twiml/dial>.
newtype Dial = Dial { fromDial :: Twiml' }

dial :: (Functor f, f a :/~ GatherNoun a, Twiml t)
     => DialAttributes
     -> Either DialNoun String
     -> f t
     -> f Dial
dial options noun = fmap (Dial . Twiml' . Fix . DialF options noun . fromTwiml' . toTwiml')

-- | The @\<Enqueue\>@ verb enqueues the current call in a call queue. See
-- <https://www.twilio.com/docs/api/twiml/enqueue>.
newtype Enqueue = Enqueue { fromEnqueue :: Twiml' }

enqueue :: (Functor f, f a :/~ GatherNoun a, Twiml t)
        => EnqueueAttributes -> f t -> f Enqueue
enqueue options = fmap (Enqueue . Twiml' . Fix . EnqueueF options . fromTwiml' . toTwiml')

-- | The @\<Leave\>@ verb transfers control of a call that is in a queue so that
-- the caller exits the queue and execution continues with the next verb after
-- the original @\<Enqueue\>@. See
-- <https://www.twilio.com/docs/api/twiml/leave>.
newtype Leave = Leave { fromLeave :: Twiml' }

leave :: (f a :/~ GatherNoun a) => (forall a. a -> f a) -> f Leave
leave f = f . Leave . Twiml' $ Fix LeaveF

instance Twiml Leave where
  toTwiml' = fromLeave

-- | The @\<Hangup\>@ verb ends a call. See
-- <https://www.twilio.com/docs/api/twiml/hangup>.
newtype Hangup = Hangup { fromHangup :: Twiml' }

hangup :: (f a :/~ GatherNoun a) => (forall a. a -> f a) -> f Hangup
hangup f = f . Hangup . Twiml' $ Fix HangupF

instance Twiml Hangup where
  toTwiml' = fromHangup

-- | The @\<Redirect\>@ verb transfers control of a call to the TwiML at a
-- different URL. See <https://www.twilio.com/docs/api/twiml/redirect>.
newtype Redirect = Redirect { fromRedirect :: Twiml' }

instance Twiml Redirect where
  toTwiml' = fromRedirect

redirect :: (f a :/~ GatherNoun a) => (forall a. a -> f a) -> URL -> f Redirect
redirect f = f . Redirect . Twiml' . Fix . RedirectF . RedirectAttributes Nothing

redirect' :: (f a :/~ GatherNoun a)
          => (forall a. a -> f a) -> Method -> URL -> f Redirect
redirect' f method = f . Redirect . Twiml' . Fix . RedirectF . RedirectAttributes (Just method)
-- | The reason attribute takes the values "rejected" and "busy." This tells
-- Twilio what message to play when rejecting a call. Selecting "busy" will play
-- a busy signal to the caller, while selecting "rejected" will play a standard
-- not-in-service response.
-- See <https://www.twilio.com/docs/api/twiml/reject#attributes-reason>
data Reason = Rejected | Busy

instance Show Reason where
  show Rejected = "rejected"
  show Busy     = "busy"

-- | The @\<Reject\>@ verb rejects an incoming call to your Twilio number
-- without billing you. See
-- <https://www.twilio.com/docs/api/2010-04-01/twiml/reject>.
newtype Reject = Reject { fromReject :: Twiml' }

instance Twiml Reject where
  toTwiml' = fromReject

reject :: (f a :/~ GatherNoun a) => (forall a. a -> f a) -> f Reject
reject f = f . Reject . Twiml' . Fix . RejectF $ RejectAttributes Nothing

reject' :: (f a :/~ GatherNoun a) => (forall a. a -> f a) -> Reason -> f Reject
reject' f = f . Reject . Twiml' . Fix . RejectF . RejectAttributes . Just

-- | The @\<Pause\>@ verb waits silently for a specific number of seconds. See
-- <https://www.twilio.com/docs/api/twiml/pause>.
newtype Pause = Pause { fromPause :: Twiml' }

instance Twiml Pause where
  toTwiml' = fromPause

pause :: (Functor f, f a :/~ GatherNoun a, Twiml t) => f t -> f Pause
pause = fmap (Pause . Twiml' . Fix . PauseF (PauseAttributes Nothing) . fromTwiml' . toTwiml')

pause' :: (Functor f, f a :/~ GatherNoun a, Twiml t)
       => Natural -> f t -> f Pause
pause' duration = fmap (Pause . Twiml' . Fix . PauseF (PauseAttributes $ Just duration) . fromTwiml' . toTwiml')
