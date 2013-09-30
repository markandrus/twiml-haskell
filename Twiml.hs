module Twiml
  ( Twiml
  , Response
  , response
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

import Data.Maybe (catMaybes)
import Data.Natural (Natural)
import Data.Traversable (Traversable)
import Text.XML.HXT.Core (ArrowXml, XmlTree, eelem, mkelem, sattr, selem)

data URL = URL { getURL :: String }

instance Show URL where
  show = getURL

data Method = GET | POST
  deriving Show

-- | TwiML is a set of instructions you can use to tell Twilio what to do
-- when you receive an incoming call or SMS. See
-- <https://www.twilio.com/docs/api/twiml>.
--
-- This library provides a number of smart constructors for creating 'Twiml', as
-- well as conversion functions for interop with other Haskell XML libraries,
-- including HXT and @xml@.
-- 
-- As an example, the following Haskell code,
--
-- @
--example
--  = sayMan \"Hello, world\"
--  . sayMan \"205-413-7965\"
--  $ hangup
-- @
--
-- Is transformed into,
--
-- @
-- \<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
-- \<Response\>
--     \<Say voice=\"man\">Hello, world\</Say\>
--     \<Hangup/\>
-- \</Response\>
-- @
class Twiml t where
  toTwimlF :: t -> TwimlF

data TwimlF
  = NullF
  | SayF SayOpts TwimlF
  | PlayF Play TwimlF
  | GatherF Gather TwimlF TwimlF
  | RecordF Record TwimlF
  | SmsF Sms TwimlF
  | DialF Dial TwimlF
  | EnqueueF Enqueue TwimlF
  | LeaveF Leave
  | HangupF Hangup
  | RedirectF Redirect
  | RejectF Reject
  | PauseF Pause TwimlF

newtype Response = Response { getResponse :: TwimlF }

response :: Twiml t => t -> Response
response = Response . toTwimlF

{-
toXmlArrow :: ArrowXml a => Response -> [n a XmlTree]
toXmlArrow (Response (Twiml' (Fix (NullF)))) = []
toXmlArrow (Response (Twiml' (Fix ((SayF _ a))))) = []
toXmlArrow (Response (Twiml' (Fix ((PlayF _ a))))) = []
toXmlArrow (Response (Twiml' (Fix ((GatherF _ a b))))) = []
toXmlArrow (Response (Twiml' (Fix ((RecordF _ a))))) = []
toXmlArrow (Response (Twiml' (Fix ((SmsF _ a))))) = []
toXmlArrow (Response (Twiml' (Fix ((DialF _ a))))) = []
toXmlArrow (Response (Twiml' (Fix ((EnqueueF _ a))))) = []
toXmlArrow (Response (Twiml' (Fix ((LeaveF Leave))))) = []
toXmlArrow (Response (Twiml' (Fix ((HangupF Hangup))))) = []
toXmlArrow (Response (Twiml' (Fix ((RedirectF _))))) = []
toXmlArrow (Response (Twiml' (Fix ((RejectF _))))) = []
toXmlArrow (Response (Twiml' (Fix ((PauseF _ a))))) = []
-}

toXml :: Twiml t => t -> TwimlF
toXml = toTwimlF

{-
instance Twiml t => Show t where
  show = (\t -> unlines . (flip runLA ()) $ (root [] $ toXml t) >>> writeDocumentToString [])
-}

example
  = sayMan   "Hello, world"
  . sayWoman "Goodybye, world"
  $ ()

-- | Useful for terminating 'Twiml'.
instance Twiml () where
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

data SayOpts = SayOpts
  { voice      :: Maybe Voice
  , sayLoop    :: Maybe Natural
  , sayMessage :: String
  }
{-
  -- , saySibling :: [LA () XmlTree]
  , saySibling :: TwimlF
  }
-}

-- | The @\<Say\>@ verb converts text to speech that is read back to the caller
-- See <https://www.twilio.com/docs/api/twiml/say>.
newtype Say = Say (SayOpts, TwimlF)

instance Twiml Say where
  toTwimlF (Say (s, n)) = SayF s n
  {-
  toXml t =
    mkelem "Say" (catMaybes
       [              fmap (sattr "voice"     . fst . voiceToStrings) $ voice   t
       , join . fmap (fmap (sattr "language") . snd . voiceToStrings) $ voice   t
       ,              fmap (sattr "loop" . show)                      $ sayLoop t
       ]) [txt $ sayMessage t]
    : saySibling t
  -}

sayMan :: Twiml t => String -> t -> Say
sayMan message n = Say (SayOpts (Just $ Man Nothing) Nothing message, toXml n)

sayMan' :: Twiml t => Lang -> String -> t -> Say
sayMan' lang message n = Say (SayOpts (Just $ Man (Just lang)) Nothing message, toXml n)

sayWoman :: Twiml t => String -> t -> Say
sayWoman message n = Say (SayOpts (Just $ Woman Nothing) Nothing message, toXml n)

sayWoman' :: Twiml t => Lang -> String -> t -> Say
sayWoman' lang message n = Say (SayOpts (Just $ Woman (Just lang)) Nothing message, toXml n)

sayAlice :: Twiml t => String -> t -> Say
sayAlice message n = Say (SayOpts (Just $ Alice Nothing) Nothing message, toXml n)

sayAlice' :: Twiml t => LangAlice -> String -> t -> Say
sayAlice' lang message n = Say (SayOpts (Just $ Alice (Just lang)) Nothing message, toXml n)

-- | The @\<Play\>@ verb plays an audio file back to the caller. Twilio
-- retrieves the file from a URL that you provide. See
-- <https://www.twilio.com/docs/api/twiml/play>.
data Play = Play
  { playLoop    :: Maybe Natural
  , url         :: URL
  -- , playSibling :: [LA () XmlTree]
  , playSibling :: TwimlF
  }

instance Twiml Play where
  toTwimlF p = PlayF p $ playSibling p
  {-
  toXml t =
    mkelem "Play" (catMaybes
       [ fmap (sattr "loop" . show) $ playLoop t
    ]) [txt . getURL $ url t] 
    : playSibling t
  -}

play :: Twiml t => URL -> t -> Play
play url = Play Nothing url . toXml

play' :: Twiml t => Natural -> URL -> t -> Play
play' i url = Play (Just i) url . toXml

-- | The @\<Gather\>@ verb collects digits that a caller enters into his or her
-- telephone keypad. See <https://www.twilio.com/docs/api/twiml/gather>.
data Gather = Gather
  -- { gatherSubtree :: [LA () XmlTree]
  -- , gatherSibling :: [LA () XmlTree]
  { gatherSubtree :: TwimlF
  , gatherSibling :: TwimlF
  }

instance Twiml Gather where
  toTwimlF g = GatherF g (gatherSubtree g) (gatherSibling g)
  {-
  toXml t = mkelem "Gather" [] (gatherSubtree t) : gatherSibling t
  -}

class Twiml t => GatherNoun t

instance GatherNoun Say

instance GatherNoun Play

instance GatherNoun Pause

gather :: (Twiml n, GatherNoun n, Twiml t) => n -> t -> Gather
gather n t = Gather (toXml n) (toXml t)

-- | The @\<Record\>@ verb records the caller's voice and returns to you the URL
-- of a file containing the audio recording. See
-- <https://www.twilio.com/docs/api/twiml/record>.
data Record = Record
  -- { recordSibling :: [LA () XmlTree]
  { recordSibling :: TwimlF
  }

instance Twiml Record where
  toTwimlF r = RecordF r $ recordSibling r
  {-
  toXml t = mkelem "Record" [] [] : recordSibling t
  -}

record :: Twiml t => t -> Record
record = Record . toXml

-- | The @\<Sms\>@ verb sends an SMS message to a phone number during a phone
-- call. See <https://www.twilio.com/docs/api/twiml/sms>.
data Sms = Sms
  { smsTo             :: Maybe String
  , smsFrom           :: Maybe String
  , smsAction         :: Maybe URL
  , smsMethod         :: Maybe Method
  , smsStatusCallback :: Maybe URL
  , smsMessage        :: String
  -- , smsSibling        :: [LA () XmlTree]
  , smsSibling        :: TwimlF
  }

instance Twiml Sms where
  toTwimlF s = SmsF s $ smsSibling s
  {-
  toXml t =
    mkelem "Sms" (catMaybes
       [ fmap (sattr "to"             . show) $ smsTo t
       , fmap (sattr "from"           . show) $ smsFrom t
       , fmap (sattr "action"         . getURL) $ smsAction t
       , fmap (sattr "method"         . show) $ smsMethod t
       , fmap (sattr "statusCallback" . getURL) $ smsStatusCallback t
    ]) [txt $ smsMessage t]
    : smsSibling t
  -}

sms :: Twiml t => String -> t -> Sms
sms message = Sms Nothing Nothing Nothing Nothing Nothing message . toXml

data DialNoun = DialNoun

-- | The @\<Dial\>@ verb connects the current caller to another phone. See
-- <https://www.twilio.com/docs/api/twiml/dial>.
data Dial = Dial
  { dialAction       :: Maybe URL
  , dialMethod       :: Maybe Method
  , dialTimeout      :: Maybe Natural
  , dialHangupOnStar :: Maybe Bool
  , dialTimeLimit    :: Maybe Natural
  , dialCallerId     :: Maybe String
  , dialRecord       :: Maybe Bool
  , dialNoun         :: Either DialNoun String
  -- , dialSibling      :: [LA () XmlTree]
  , dialSibling      :: TwimlF
  }

-- | The @\<Enqueue\>@ verb enqueues the current call in a call queue. See
-- <https://www.twilio.com/docs/api/twiml/enqueue>.
data Enqueue = Enqueue
  { enqueueName    :: String
  -- , enqueueSibling :: [LA () XmlTree]
  , enqueueSibling :: TwimlF
  }

-- | The @\<Leave\>@ verb transfers control of a call that is in a queue so that
-- the caller exits the queue and execution continues with the next verb after
-- the original @\<Enqueue\>@. See
-- <https://www.twilio.com/docs/api/twiml/leave>.
data Leave = Leave

instance Twiml Leave where
  toTwimlF = LeaveF
  {-
  toXml _ = [eelem "Leave"]
  -}

-- | The @\<Hangup\>@ verb ends a call. See
-- <https://www.twilio.com/docs/api/twiml/hangup>.
data Hangup = Hangup

instance Twiml Hangup where
  toTwimlF = HangupF
  {-
  toXml _ = [eelem "Hangup"]
  -}

-- | The @\<Redirect\>@ verb transfers control of a call to the TwiML at a
-- different URL. See <https://www.twilio.com/docs/api/twiml/redirect>.
data Redirect = Redirect
  { redirectMethod :: Maybe Method
  , redirectURL    :: URL
  }

instance Twiml Redirect where
  toTwimlF = RedirectF
  {-
  toXml t = [ selem "Redirect" (catMaybes
    [ fmap  (sattr "method" . show ) $ redirectMethod t
    , Just . sattr "url"    . getURL $ redirectURL    t
    ]) ]
  -}

redirect :: URL -> Redirect
redirect = Redirect Nothing

redirect' :: Method -> URL -> Redirect
redirect' method = Redirect (Just method)

data Reason = Rejected | Busy

instance Show Reason where
  show Rejected = "rejected"
  show Busy     = "busy"

-- | The @\<Reject\>@ verb rejects an incoming call to your Twilio number
-- without billing you. See
-- <https://www.twilio.com/docs/api/2010-04-01/twiml/reject>.
data Reject = Reject { rejectReason :: Maybe Reason }

instance Twiml Reject where
  toTwimlF = RejectF
  {-
  toXml t = [ selem "Reject" (catMaybes
    [ fmap (sattr "reason" . show) $ rejectReason t
    ]) ]
  -}

reject :: Reject
reject = Reject Nothing

reject' :: Reason -> Reject
reject' = Reject . Just

-- | The @\<Pause\>@ verb waits silently for a specific number of seconds. See
-- <https://www.twilio.com/docs/api/twiml/pause>.
data Pause = Pause
  { pauseDuration :: Maybe Natural
  -- , pauseSibling  :: [LA () XmlTree]
  , pauseSibling  :: TwimlF
  }

instance Twiml Pause where
  toTwimlF p = PauseF p $ pauseSibling p
  {-
  toXml t =
    selem "Pause" (catMaybes
      [ fmap (sattr "FUCK" . show) $ pauseDuration t ]) : pauseSibling t
  -}

pause :: Twiml t => t -> Pause
pause = Pause Nothing . toXml

pause' :: Twiml t => Natural -> t -> Pause
pause' duration = Pause (Just duration) . toXml
