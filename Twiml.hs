{-#LANGUAGE NoMonomorphismRestriction #-}

module Twiml
  ( Twiml
  , Null
  , Response
  , response
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
  , GatherNoun
  , gather
  -- *** Nouns
  , gNull
  , gSayMan
  , gSayMan'
  , gSayWoman
  , gSayWoman'
  , gSayAlice
  , gSayAlice'
  , gPlay
  , gPlay'
  , gPause
  , gPause'
  -- ** @\<Record\>@
  , Record
  , record
  -- ** @\<Sms\>@
  , Sms
  , sms
  -- ** @\<Dial\>@
  , Dial
  , DialNoun
  -- * Secondary Verbs
  -- ** @\<Enqueue\>@
  , Enqueue
  -- ** @\<Leave\>@
  , Leave
  -- ** @\<Hangup\>@
  , Hangup
  -- ** @\<Redirect\>@
  , Redirect
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
import Data.Maybe (catMaybes)
import Data.Natural (Natural)
import Text.XML.HXT.Core

data URL = URL { getURL :: String }

instance Show URL where
  show = getURL

data Method = GET | POST
  deriving Show

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

class Twiml t where
  toTwimlF :: t -> TwimlF

data TwimlF
  = NullF
  | SayF
      { sayVoice          :: Maybe Voice
      , sayLoop           :: Maybe Natural
      , sayMessage        :: String
      , sayNext           :: TwimlF
      }
  | PlayF
      { playLoop          :: Maybe Natural
      , playURL           :: URL
      , playNext          :: TwimlF
      }
  | GatherF
      { gatherNouns       :: TwimlF
      , gatherNext        :: TwimlF
      }
  | RecordF
      { recordNext        :: TwimlF
      }
  | SmsF
      { smsTo             :: Maybe String
      , smsFrom           :: Maybe String
      , smsAction         :: Maybe URL
      , smsMethod         :: Maybe Method
      , smsStatusCallback :: Maybe URL
      , smsMessage        :: String
      , smsNext           :: TwimlF
      }
  | DialF
      { dialAction        :: Maybe URL
      , dialMethod        :: Maybe Method
      , dialTimeout       :: Maybe Natural
      , dialHangupOnStar  :: Maybe Bool
      , dialTimeLimit     :: Maybe Natural
      , dialCallerId      :: Maybe String
      , dialRecord        :: Maybe Bool
      , dialNoun          :: Either DialNoun String
      , dialNext          :: TwimlF
      }
  | EnqueueF
      { enqueueName       :: String
      , enqueueNext       :: TwimlF
      }
  | LeaveF
  | HangupF
  | RedirectF
      { redirectMethod    :: Maybe Method
      , redirectURL       :: URL
      }
  | RejectF
      { rejectReason      :: Maybe Reason }
  | PauseF
      { pauseLength       :: Maybe Natural
      , pauseNext         :: TwimlF
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

response :: Twiml t => t -> Response
response = Response . toTwimlF

-- | Convert a 'Response' into an @ArrowXml@ to be used with HXT. See
-- <http://hackage.haskell.org/package/hxt>.
toArrowXml :: ArrowXml a => Response -> a n XmlTree
toArrowXml = root [] . return . selem "Response" . go . fromResponse where
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
  -- FIXME: ...
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
      ) [ txt $ smsMessage t ] : go (smsNext t)
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
  -- FIXME: ...
  go t@(PauseF {}) =
    selem "Pause"
      ( catMaybes
        [ fmap (sattr "length" . show) $ pauseLength t ]
      ) : go (pauseNext t)

-- | Useful for terminating 'Twiml'.
newtype Null = Null { getNull :: () }

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
 
sayMan :: Twiml t => String -> t -> Say
sayMan message
  = Say . SayF (Just $ Man Nothing) Nothing message . toTwimlF

sayMan' :: Twiml t => Lang -> String -> t -> Say
sayMan' lang message
  = Say . SayF (Just . Man $ Just lang) Nothing message . toTwimlF

sayWoman :: Twiml t => String -> t -> Say
sayWoman message
  = Say . SayF (Just $ Woman Nothing) Nothing message . toTwimlF

sayWoman' :: Twiml t => Lang -> String -> t -> Say
sayWoman' lang message
  = Say . SayF (Just . Woman $ Just lang) Nothing message . toTwimlF

sayAlice :: Twiml t => String -> t -> Say
sayAlice message
  = Say . SayF (Just $ Alice Nothing) Nothing message . toTwimlF

sayAlice' :: Twiml t => LangAlice -> String -> t -> Say
sayAlice' lang message
  = Say . SayF (Just . Alice $ Just lang) Nothing message . toTwimlF

-- | The @\<Play\>@ verb plays an audio file back to the caller. Twilio
-- retrieves the file from a URL that you provide. See
-- <https://www.twilio.com/docs/api/twiml/play>.
newtype Play = Play { fromPlay :: TwimlF }

instance Twiml Play where
  toTwimlF = fromPlay

play :: Twiml t => URL -> t -> Play
play url = Play . PlayF Nothing url . toTwimlF

play' :: Twiml t => Natural -> URL -> t -> Play
play' i url = Play . PlayF (Just i) url . toTwimlF

-- | The @\<Gather\>@ verb collects digits that a caller enters into his or her
-- telephone keypad. See <https://www.twilio.com/docs/api/twiml/gather>.
--
-- You can nest @\<Say\>@, @\<Play\>@, and @\<Pause\>@ in @\<Gather\>@. See
-- <https://www.twilio.com/docs/api/twiml/gather#nesting-rules>.
--
-- Nesting rules are made explicit via the 'GatherNoun' constructor, and the
-- following section lifts previously-defined functions such as 'sayMan' and
-- 'play' into the 'GatherNoun' type.
newtype Gather = Gather { fromGather :: TwimlF }

instance Twiml Gather where
  toTwimlF = fromGather

newtype GatherNoun t = GatherNoun { fromGatherNoun :: t }

instance Twiml t => Twiml (GatherNoun t) where
  toTwimlF = toTwimlF . fromGatherNoun

gather :: (Twiml n, Twiml t) => GatherNoun n -> t -> Gather
gather (GatherNoun n) = Gather . GatherF (toTwimlF n) . toTwimlF

gNull :: GatherNoun Null
gNull = GatherNoun $ Null ()

gSayMan :: Twiml t => String -> GatherNoun t -> GatherNoun Say
gSayMan message = GatherNoun . sayMan message . fromGatherNoun

gSayMan' :: Twiml t => Lang -> String -> GatherNoun t -> GatherNoun Say
gSayMan' lang message = GatherNoun . sayMan' lang message . fromGatherNoun

gSayWoman :: Twiml t => String -> GatherNoun t -> GatherNoun Say
gSayWoman message = GatherNoun . sayWoman message . fromGatherNoun

gSayWoman' :: Twiml t => Lang -> String -> GatherNoun t -> GatherNoun Say
gSayWoman' lang message = GatherNoun . sayWoman' lang message . fromGatherNoun

gSayAlice :: Twiml t => String -> GatherNoun t -> GatherNoun Say
gSayAlice message = GatherNoun . sayAlice message . fromGatherNoun

gSayAlice' :: Twiml t => LangAlice -> String -> GatherNoun t -> GatherNoun Say
gSayAlice' lang message = GatherNoun . sayAlice' lang message . fromGatherNoun

gPlay :: Twiml t => URL -> GatherNoun t -> GatherNoun Play
gPlay url = GatherNoun . play url . fromGatherNoun

gPlay' :: Twiml t => Natural -> URL -> GatherNoun t -> GatherNoun Play
gPlay' i url = GatherNoun . play' i url . fromGatherNoun

gPause :: Twiml t => GatherNoun t -> GatherNoun Pause
gPause = GatherNoun . pause . fromGatherNoun

gPause' :: Twiml t => Natural -> GatherNoun t -> GatherNoun Pause
gPause' duration = GatherNoun . pause' duration . fromGatherNoun

-- | The @\<Record\>@ verb records the caller's voice and returns to you the URL
-- of a file containing the audio recording. See
-- <https://www.twilio.com/docs/api/twiml/record>.
newtype Record = Record { fromRecord :: TwimlF }

instance Twiml Record where
  toTwimlF = fromRecord

-- FIXME: ...
record :: Twiml t => t -> Record
record = Record . RecordF . toTwimlF

-- | The @\<Sms\>@ verb sends an SMS message to a phone number during a phone
-- call. See <https://www.twilio.com/docs/api/twiml/sms>.
newtype Sms = Sms { fromSms :: TwimlF }

instance Twiml Sms where
  toTwimlF = fromSms

sms :: Twiml t => String -> t -> Sms
sms message = Sms . SmsF Nothing Nothing Nothing Nothing Nothing message . toTwimlF

-- FIXME: ...
data DialNoun = DialNoun

-- | The @\<Dial\>@ verb connects the current caller to another phone. See
-- <https://www.twilio.com/docs/api/twiml/dial>.
newtype Dial = Dial { fromDial :: TwimlF }

-- | The @\<Enqueue\>@ verb enqueues the current call in a call queue. See
-- <https://www.twilio.com/docs/api/twiml/enqueue>.
newtype Enqueue = Enqueue { fromEnqueue :: TwimlF }

-- | The @\<Leave\>@ verb transfers control of a call that is in a queue so that
-- the caller exits the queue and execution continues with the next verb after
-- the original @\<Enqueue\>@. See
-- <https://www.twilio.com/docs/api/twiml/leave>.
newtype Leave = Leave { fromLeave :: TwimlF }

leave :: Leave
leave = Leave $ LeaveF

instance Twiml Leave where
  toTwimlF = fromLeave

-- | The @\<Hangup\>@ verb ends a call. See
-- <https://www.twilio.com/docs/api/twiml/hangup>.
newtype Hangup = Hangup { fromHangup :: TwimlF }

hangup = Hangup $ HangupF

instance Twiml Hangup where
  toTwimlF = fromHangup

-- | The @\<Redirect\>@ verb transfers control of a call to the TwiML at a
-- different URL. See <https://www.twilio.com/docs/api/twiml/redirect>.
newtype Redirect = Redirect { fromRedirect :: TwimlF }

instance Twiml Redirect where
  toTwimlF = fromRedirect

redirect :: URL -> Redirect
redirect = Redirect . RedirectF Nothing

redirect' :: Method -> URL -> Redirect
redirect' method = Redirect . RedirectF (Just method)

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

reject :: Reject
reject = Reject $ RejectF Nothing

reject' :: Reason -> Reject
reject' = Reject . RejectF . Just

-- | The @\<Pause\>@ verb waits silently for a specific number of seconds. See
-- <https://www.twilio.com/docs/api/twiml/pause>.
newtype Pause = Pause { fromPause :: TwimlF }

instance Twiml Pause where
  toTwimlF = fromPause

pause :: Twiml t => t -> Pause
pause = Pause . PauseF Nothing . toTwimlF

pause' :: Twiml t => Natural -> t -> Pause
pause' duration = Pause . PauseF (Just duration) . toTwimlF
