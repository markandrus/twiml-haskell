{-#LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies,
  MultiParamTypeClasses, NoMonomorphismRestriction, Rank2Types, TypeOperators,
  UndecidableInstances #-}

module Twiml
  ( Twiml
  , Null
  , Response
  , respond
  , toArrowXml
  -- * Primary Verbs
  -- ** @\<Say\>@
  , Say
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
  , play
  , play'
  -- ** @\<Gather\>@
  , Gather
  -- , GatherNoun
  , gatherNull
  , gather
  -- ** @\<Record\>@
  , Record
  , record
  -- ** @\<Sms\>@
  , Sms
  , sms
  , sms'
  -- ** @\<Dial\>@
  , Dial
  , DialNoun
  , dial
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
  , redirect
  , redirect'
  -- ** @\<Reject\>@
  , Reject
  , Reason
  , reject
  , reject'
  -- ** @\<Pause\>@
  , Pause
  , pause
  , pause'
  ) where

import Control.Arrow ((>>>))
import Data.Functor.Identity (Identity(..))
import Data.Maybe (catMaybes)
import Data.Natural (Natural)
import Text.XML.HXT.Core

data URL = URL { getURL :: String }

instance Show URL where
  show = getURL

data Method = GET | POST
  deriving Show

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
  toTwimlF :: t -> TwimlF

newtype Twiml' p t = Twiml' { fromTwiml' :: t }

data TwimlF
  = NullF
  | SayF
      { sayVoice                 :: Maybe Voice
      , sayLoop                  :: Maybe Natural
      , sayMessage               :: String
      , sayNext                  :: TwimlF
      }
  | PlayF
      { playLoop                 :: Maybe Natural
      , playURL                  :: URL
      , playNext                 :: TwimlF
      }
  | GatherF
      { gatherAction             :: Maybe URL
      , gatherMethod             :: Maybe Method
      , gatherTimeout            :: Maybe Natural
      , gatherFinishOnKey        :: Maybe Key
      , gatherNumDigits          :: Maybe Natural
      , gatherNouns              :: TwimlF
      , gatherNext               :: TwimlF
      }
  | RecordF
      { recordAction             :: Maybe URL
      , recordMethod             :: Maybe Method
      , recordTimeout            :: Maybe Natural
      , recordFinishOnKey        :: Maybe Key
      , recordMaxLength          :: Maybe Natural
      , recordTranscribe         :: Maybe Bool
      , recordTranscribeCallback :: Maybe URL
      , recordPlayBeep           :: Maybe Bool
      , recordNext               :: TwimlF
      }
  | SmsF
      { smsTo                    :: Maybe String
      , smsFrom                  :: Maybe String
      , smsAction                :: Maybe URL
      , smsMethod                :: Maybe Method
      , smsStatusCallback        :: Maybe URL
      , smsNoun                  :: String
      , smsNext                  :: TwimlF
      }
  | DialF
      { dialAction               :: Maybe URL
      , dialMethod               :: Maybe Method
      , dialTimeout              :: Maybe Natural
      , dialHangupOnStar         :: Maybe Bool
      , dialTimeLimit            :: Maybe Natural
      , dialCallerId             :: Maybe String
      , dialRecord               :: Maybe Bool
      , dialNoun                 :: Either DialNoun String
      , dialNext                 :: TwimlF
      }
  | EnqueueF
      { enqueueName              :: String
      , enqueueNext              :: TwimlF
      }
  | LeaveF
  | HangupF
  | RedirectF
      { redirectMethod           :: Maybe Method
      , redirectURL              :: URL
      }
  | RejectF
      { rejectReason             :: Maybe Reason }
  | PauseF
      { pauseLength              :: Maybe Natural
      , pauseNext                :: TwimlF
      }

-- | The root element of Twilio's XML Markup is the @\<Response\>@ element. In
-- any TwiML response to a Twilio request, all verb elements must be nested
-- within this element. Any other structure is considered invalid. See
-- <https://www.twilio.com/docs/api/twiml/your_response#response-element>.
newtype Response = Response { fromResponse :: TwimlF }

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
respond f = Response . toTwimlF . f

-- | Convert a 'Response' into an @ArrowXml@ to be used with HXT. See
-- <http://hackage.haskell.org/package/hxt>.
toArrowXml :: ArrowXml a => Response -> a n XmlTree
toArrowXml = root [] . return . selem "Response" . go . fromResponse where
  -- FIXME: We're not printing the correct TwiML for each tag!
  go NullF = []
  go t@(SayF {}) =
    mkelem "Say"
      ( catMaybes
        [ fmap (sattr "voice" . fst . voiceToStrings) $ sayVoice t
        , sayVoice t >>= fmap (sattr "language") . snd . voiceToStrings
        , fmap (sattr "loop" . show) $ sayLoop t
        ]
      ) [ txt $ sayMessage t ] : go (sayNext t)
  go t@(PlayF {}) =
    mkelem "Play"
      ( catMaybes
        [ fmap (sattr "loop" . show) $ playLoop t ]
      ) [ txt . getURL $ playURL t ] : go (playNext t)
  go t@(GatherF {}) =
    mkelem "Gather" [] (go $ gatherNouns t) : go (gatherNext t)
  go t@(RecordF {}) =
    mkelem "Record" [] [] : go (recordNext t)
  go t@(SmsF {}) =
    mkelem "Sms"
      ( catMaybes
        [ fmap (sattr "to"             . show  ) $ smsTo             t
        , fmap (sattr "from"           . show  ) $ smsFrom           t
        , fmap (sattr "action"         . getURL) $ smsAction         t
        , fmap (sattr "method"         . show  ) $ smsMethod         t
        , fmap (sattr "statusCallback" . getURL) $ smsStatusCallback t
        ]
      ) [ txt $ smsNoun t ] : go (smsNext t)
  go t@(HangupF {}) =
    [ eelem "Hangup" ]
  go t@(RedirectF {}) =
    [ selem "Redirect"
      ( catMaybes
        [ fmap  (sattr "method" . show)  $ redirectMethod t
        , Just . sattr "url"    . getURL $ redirectURL    t ]
      )
    ]
  go t@(RejectF {}) =
    [ selem "Reject"
      ( catMaybes
        [ fmap (sattr "reason" . show) $ rejectReason t ]
      )
    ]
  go t@(PauseF {}) =
    selem "Pause"
      ( catMaybes
        [ fmap (sattr "length" . show) $ pauseLength t ]
      ) : go (pauseNext t)

-- | Useful for terminating 'Twiml'.
newtype Null = Null { fromNull :: TwimlF }

null :: Identity Null
null = Identity $ Null NullF

instance Twiml Null where
  toTwimlF _ = NullF

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

-- | The @\<Say\>@ verb converts text to speech that is read back to the caller
-- See <https://www.twilio.com/docs/api/twiml/say>.
newtype Say = Say { fromSay :: TwimlF }

instance Twiml Say where
  toTwimlF = fromSay

sayMan :: (Functor f, Twiml t) => String -> f t -> f Say
sayMan message
  = fmap (Say . SayF (Just $ Man Nothing) Nothing message . toTwimlF)

sayMan' :: (Functor f, Twiml t) => Lang -> String -> f t -> f Say
sayMan' lang message
  = fmap (Say . SayF (Just . Man $ Just lang) Nothing message . toTwimlF)

sayWoman :: (Functor f, Twiml t) => String -> f t -> f Say
sayWoman message
  = fmap (Say . SayF (Just $ Woman Nothing) Nothing message . toTwimlF)

sayWoman' :: (Functor f, Twiml t) => Lang -> String -> f t -> f Say
sayWoman' lang message
  = fmap (Say . SayF (Just . Woman $ Just lang) Nothing message . toTwimlF)

sayAlice :: (Functor f, Twiml t) => String -> f t -> f Say
sayAlice message
  = fmap (Say . SayF (Just $ Alice Nothing) Nothing message . toTwimlF)

sayAlice' :: (Functor f, Twiml t) => LangAlice -> String -> f t -> f Say
sayAlice' lang message
  = fmap (Say . SayF (Just . Alice $ Just lang) Nothing message . toTwimlF)

-- | The @\<Play\>@ verb plays an audio file back to the caller. Twilio
-- retrieves the file from a URL that you provide. See
-- <https://www.twilio.com/docs/api/twiml/play>.
newtype Play = Play { fromPlay :: TwimlF }

instance Twiml Play where
  toTwimlF = fromPlay

play :: (Functor f, Twiml t) => URL -> f t -> f Play
play url = fmap (Play . PlayF Nothing url . toTwimlF)

play' :: (Functor f, Twiml t) => Natural -> URL -> f t -> f Play
play' i url = fmap (Play . PlayF (Just i) url . toTwimlF)

-- | The @\<Gather\>@ verb collects digits that a caller enters into his or her
-- telephone keypad. See <https://www.twilio.com/docs/api/twiml/gather>.
--
-- You can nest @\<Say\>@, @\<Play\>@, and @\<Pause\>@ in @\<Gather\>@. See
-- <https://www.twilio.com/docs/api/twiml/gather#nesting-rules>.
--
-- Nesting rules are made explicit via an unexported functor, 'GatherNoun'.
newtype Gather = Gather { fromGather :: TwimlF }

instance Twiml Gather where
  toTwimlF = fromGather

newtype GatherNoun t = GatherNoun { fromGatherNoun :: t }

instance Functor GatherNoun where
  fmap f = GatherNoun . f . fromGatherNoun

-- TODO: Document this.
gather :: (Functor f, f a :/~ GatherNoun a, Twiml n, Twiml t)
       => Maybe URL
       -> Maybe Method
       -> Maybe Natural
       -> Maybe Key
       -> Maybe Natural
       -> GatherNoun n
       -> f t
       -> f Gather
gather a b c d e (GatherNoun n) = fmap (Gather . GatherF a b c d e (toTwimlF n) . toTwimlF)

gatherNull :: GatherNoun Null
gatherNull = GatherNoun $ Null NullF

-- | The @\<Record\>@ verb records the caller's voice and returns to you the URL
-- of a file containing the audio recording. See
-- <https://www.twilio.com/docs/api/twiml/record>.
newtype Record = Record { fromRecord :: TwimlF }

instance Twiml Record where
  toTwimlF = fromRecord

-- TODO: Document this.
record :: (Functor f, f a :/~ GatherNoun a, Twiml t)
       => Maybe URL
       -> Maybe Method
       -> Maybe Natural
       -> Maybe Key
       -> Maybe Natural
       -> Maybe Bool
       -> Maybe URL
       -> Maybe Bool
       -> f t
       -> f Record
record a b c d e f g h = fmap (Record . RecordF a b c d e f g h . toTwimlF)

-- | The @\<Sms\>@ verb sends an SMS message to a phone number during a phone
-- call. See <https://www.twilio.com/docs/api/twiml/sms>.
newtype Sms = Sms { fromSms :: TwimlF }

instance Twiml Sms where
  toTwimlF = fromSms

sms' :: (Functor f, f a :/~ GatherNoun a, Twiml t) => String -> f t -> f Sms
sms' noun = fmap (Sms . SmsF Nothing Nothing Nothing Nothing Nothing noun . toTwimlF)

-- TODO: Document this.
sms :: (Functor f, f a :/~ GatherNoun a, Twiml t)
    => Maybe String
    -> Maybe String
    -> Maybe URL
    -> Maybe Method
    -> Maybe URL
    -> String
    -> f t
    -> f Sms
sms a b c d e f = fmap (Sms . SmsF a b c d e f . toTwimlF)

-- FIXME: Unimplemented!
data DialNoun = DialNoun

-- | The @\<Dial\>@ verb connects the current caller to another phone. See
-- <https://www.twilio.com/docs/api/twiml/dial>.
newtype Dial = Dial { fromDial :: TwimlF }

-- TODO: Document this.
dial :: (Functor f, f a :/~ GatherNoun a, Twiml t)
     => Maybe URL
     -> Maybe Method
     -> Maybe Natural
     -> Maybe Bool
     -> Maybe Natural
     -> Maybe String
     -> Maybe Bool
     -> Either DialNoun String
     -> f t
     -> f Dial
dial a b c d e f g h = fmap (Dial . DialF a b c d e f g h . toTwimlF)

-- | The @\<Enqueue\>@ verb enqueues the current call in a call queue. See
-- <https://www.twilio.com/docs/api/twiml/enqueue>.
newtype Enqueue = Enqueue { fromEnqueue :: TwimlF }

enqueue :: (Functor f, f a :/~ GatherNoun a, Twiml t)
        => String -> f t -> f Enqueue
enqueue name = fmap (Enqueue . EnqueueF name . toTwimlF)

-- | The @\<Leave\>@ verb transfers control of a call that is in a queue so that
-- the caller exits the queue and execution continues with the next verb after
-- the original @\<Enqueue\>@. See
-- <https://www.twilio.com/docs/api/twiml/leave>.
newtype Leave = Leave { fromLeave :: TwimlF }

leave :: (f a :/~ GatherNoun a) => (forall a. a -> f a) -> f Leave
leave f = f $ Leave LeaveF

instance Twiml Leave where
  toTwimlF = fromLeave

-- | The @\<Hangup\>@ verb ends a call. See
-- <https://www.twilio.com/docs/api/twiml/hangup>.
newtype Hangup = Hangup { fromHangup :: TwimlF }

hangup :: (f a :/~ GatherNoun a) => (forall a. a -> f a) -> f Hangup
hangup f = f $ Hangup HangupF

instance Twiml Hangup where
  toTwimlF = fromHangup

-- | The @\<Redirect\>@ verb transfers control of a call to the TwiML at a
-- different URL. See <https://www.twilio.com/docs/api/twiml/redirect>.
newtype Redirect = Redirect { fromRedirect :: TwimlF }

instance Twiml Redirect where
  toTwimlF = fromRedirect

redirect :: (f a :/~ GatherNoun a) => (forall a. a -> f a) -> URL -> f Redirect
redirect f = f . Redirect . RedirectF Nothing

redirect' :: (f a :/~ GatherNoun a)
          => (forall a. a -> f a) -> Method -> URL -> f Redirect
redirect' f method = f . Redirect . RedirectF (Just method)

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
newtype Reject = Reject { fromReject :: TwimlF }

instance Twiml Reject where
  toTwimlF = fromReject

reject :: (f a :/~ GatherNoun a) => (forall a. a -> f a) -> f Reject
reject f = f . Reject $ RejectF Nothing

reject' :: (f a :/~ GatherNoun a) => (forall a. a -> f a) -> Reason -> f Reject
reject' f = f . Reject . RejectF . Just

-- | The @\<Pause\>@ verb waits silently for a specific number of seconds. See
-- <https://www.twilio.com/docs/api/twiml/pause>.
newtype Pause = Pause { fromPause :: TwimlF }

instance Twiml Pause where
  toTwimlF = fromPause

pause :: (Functor f, f a :/~ GatherNoun a, Twiml t) => f t -> f Pause
pause = fmap (Pause . PauseF Nothing . toTwimlF)

pause' :: (Functor f, f a :/~ GatherNoun a, Twiml t)
       => Natural -> f t -> f Pause
pause' duration = fmap (Pause . PauseF (Just duration) . toTwimlF)
