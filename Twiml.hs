module Twiml
  (
  -- $example
    Twiml
  -- * Primary Verbs
  -- ** @\<Say\>@
  -- $say
  , Voice(..)
  , Lang(..)
  , LangAlice(..)
  , say
  , sayMan
  , sayMan'
  , sayWoman
  , sayWoman'
  , sayAlice
  , sayAlice'
  -- ** @\<Play\>@
  -- $play
  , play
  , playLoop
  -- ** @\<Gather\>@
  , gather
  -- ** @\<Record\>@
  , record
  -- ** @\<Reject\>@
  , reject
  , Reason(..)
  -- ** @\<Pause\>@
  , pause
  -- ** @\<Sms\>@
  , sms
  -- ** @\<Dial\>@
  , dial
  , DialNoun(..)
  -- * Secondary Verbs
  -- ** @\<Enqueue\>@
  , enqueue
  -- ** @\<Leave\>@
  , leave
  -- ** @\<Hangup\>@
  , hangup
  -- ** @\<Redirect\>@
  , redirect
  , end
  -- * Types
  , Method(..)
  , URL
  , parseURL
  , toXml
  ) where

import Control.Applicative(Applicative(..), (<$>))
import Data.Maybe (fromMaybe, fromJust)
import Network.URI (URI(..), parseURIReference)
import Text.XML.HXT.Core

-- $example TwiML is a set of instructions you can use to tell Twilio what to do
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

data URL = URL { getURL :: String }

instance Show URL where
  show = getURL

parseURL :: String -> Maybe URL
parseURL url = parseURIReference url
           >>= (\uri -> if isHttp uri then Just (URL url) else Nothing)

data Method = GET | POST
  deriving Show

{- Utilities -}

-- | Checks whether a given @RequestMethod@ is supported, i.e. is either @GET@
-- or @POST@.
suppMeth :: a -> Bool
suppMeth _  = True

-- | Checks whether a @URI@'s scheme, if any, is one of @"http:"@ or @"https:"@.
isHttp :: URI -> Bool
isHttp uri = case uriScheme uri of
  ""       -> True
  "http:"  -> True
  "https:" -> True
  _        -> False

-- | Perform an action if the given @URI@ satisfies 'isHttp'.
ifHttp :: URI -> a -> Maybe a
ifHttp uri = if isHttp uri then Just else const Nothing
data DialNoun
  = Number String
  | Client String
  | Sip String
  | Conference String
  | Queue String

{- Datatypes -}

-- | A 'Reason' for use with @\<Reject\>@.
data Reason
  = Rejected
  | Busy

data Twiml
  = Null
  | Say String Twiml
  | Play (Maybe Int) URL Twiml
  | Gather Twiml Twiml
  | Record Twiml
  | Sms { smsTo             :: Maybe String
        , smsFrom           :: Maybe String
        , smsAction         :: Maybe URL
        , smsMethod         :: Maybe Method
        , smsStatusCallback :: Maybe URL
        , smsNoun           :: String
        , smsNext           :: Twiml }
  | Dial { dialAction       :: Maybe URL
         , dialMethod       :: Maybe Method
         , dialTimeout      :: Maybe Int
         , dialHangupOnStar :: Maybe Bool
         , dialTimeLimit    :: Maybe Int
         , dialCallerId     :: Maybe String
         , dialRecord       :: Maybe Bool
         , dialNoun         :: Either DialNoun String
         , dialNext         :: Twiml }
  | Enqueue String Twiml
  | Leave
  | Hangup
  | Redirect (Maybe Method) URL
  | Reject (Maybe Reason)
  | Pause (Maybe Int) Twiml

instance Show Twiml where
  show twiml = concat $ runLA (toXml twiml >>> writeDocumentToString []) ()

{- Smart Constructors -}

-- | Voices supported by @\<Say\>@.
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

-- $say The @\<Say\>@ verb converts text to speech that is read back to the
-- caller. See <https://www.twilio.com/docs/api/twiml/say>.

say :: Maybe Voice -> String -> Twiml -> Twiml
say _ str = Say str

sayMan :: String -> Twiml -> Twiml
sayMan = undefined

sayMan' :: Lang -> String -> Twiml -> Twiml
sayMan' = undefined

sayWoman :: String -> Twiml -> Twiml
sayWoman = undefined

sayWoman' :: Lang -> String -> Twiml -> Twiml
sayWoman' = undefined

sayAlice :: String ->Twiml -> Twiml
sayAlice = undefined

sayAlice' :: LangAlice -> String -> Twiml -> Twiml
sayAlice' = undefined

-- $play The @\<Play\>@ verb plays an audio file back to the caller. Twilio
-- retrieves the file from a URL that you provide. See
-- <https://www.twilio.com/docs/api/twiml/play>.

-- | This function returns @Nothing@ if the @URL@ specifies a scheme other than
-- @HTTP@ or @HTTPS@.
play :: URL -> Twiml -> Twiml
play url = Play Nothing url

-- | This function returns @Nothing@ if the loop parameter is less than zero, or
-- if the @URL@ specifies a scheme other than @HTTP@ or @HTTPS@.
playLoop :: Int -> URL -> Twiml -> Maybe Twiml
playLoop i url | i >= 0    = Just . Play (Just i) url
               | otherwise = const Nothing

-- | The @\<Gather\>@ verb collects digits that a caller enters into his or her
-- telephone keypad. See <https://www.twilio.com/docs/api/twiml/gather>.
gather :: Twiml -> Twiml -> Maybe Twiml
gather body@(Say _ _) = Just . Gather body
gather body@(Play _ _ _) = Just . Gather body
gather body@(Pause _ _) = Just . Gather body
gather _ = const Nothing

-- | The @\<Record\>@ verb records the caller's voice and returns to you the URL
-- of a file containing the audio recording. See
-- <https://www.twilio.com/docs/api/twiml/record>.
record :: Twiml -> Twiml
record = Record

-- | The @\<Sms\>@ verb sends an SMS message to a phone number during a phone
-- call. See <https://www.twilio.com/docs/api/twiml/sms>.
sms :: Maybe URL -> String -> Maybe Method -> Twiml -> Maybe Twiml
sms action msg method next
  | length msg < 160
  = Just $ Sms Nothing Nothing action method Nothing msg next
  | otherwise = Nothing

-- | The @\<Dial\>@ verb connects the current caller to another phone. See
-- <https://www.twilio.com/docs/api/twiml/dial>.
dial :: Either DialNoun String -> Twiml -> Twiml
dial = dial' Nothing Nothing Nothing Nothing Nothing Nothing Nothing

dial' :: Maybe URL
      -> Maybe Method
      -> Maybe Int
      -> Maybe Bool
      -> Maybe Int
      -> Maybe String
      -> Maybe Bool
      -> Either DialNoun String
      -> Twiml
      -> Twiml
dial' = Dial

-- | The @\<Enqueue\>@ verb enqueues the current call in a call queue. See
-- <https://www.twilio.com/docs/api/twiml/enqueue>.
enqueue :: String -> Twiml -> Twiml
enqueue queue = Enqueue queue

-- | The @\<Leave\>@ verb transfers control of a call that is in a queue so that
-- the caller exits the queue and execution continues with the next verb after
-- the original @\<Enqueue\>@. See
-- <https://www.twilio.com/docs/api/twiml/leave>.
leave :: Twiml
leave = Leave

-- | The @\<Hangup\>@ verb ends a call. See
-- <https://www.twilio.com/docs/api/twiml/hangup>.
hangup :: Twiml
hangup = Hangup

-- | The @\<Redirect\>@ verb transfers control of a call to the TwiML at a
-- different URL. See <https://www.twilio.com/docs/api/twiml/redirect>.
redirect :: Maybe Method -> URL -> Twiml
redirect = Redirect

-- | The @\<Reject\>@ verb rejects an incoming call to your Twilio number
-- without billing you. See
-- <https://www.twilio.com/docs/api/2010-04-01/twiml/reject>.
reject :: Maybe Reason -> Twiml
reject = Reject

-- | The @\<Pause\>@ verb waits silently for a specific number of seconds. See
-- <https://www.twilio.com/docs/api/twiml/pause>.
pause :: Maybe Int -> Twiml -> Twiml
pause = Pause

-- | Terminate 'Twiml'.
end = Null

example
  = sayMan "Hello, world"
  . sayMan "205-413-7965"
  $ hangup

example2
  =  sayMan "Hello, world"
 <$> gather Null hangup

toXml :: ArrowXml a => Twiml -> a n XmlTree
toXml = root [] . return . selem "Response" . go []
-- toXml = selem "Response" . go []
  where
    go es Null = es
    go es (Say    str  mu) = go (es ++ [selem "Say"  [txt str]]) mu
    --go es (Play   url  mu) = go (es ++ [selem "Play" [txt url]]) mu
    go es (Gather body mu) = go (es ++ [selem "Gather" $ go []   body])
                                                                 mu
    go es (Record      mu) = go (es ++ [eelem "Record"])         mu
    --go es (Sms    str  mu) = go (es ++ [selem "Sms"  [txt str]]) mu
    --go es (Dial (Right number) mu) =
    --  go (es ++ [selem "Dial" [txt number]]) mu
    --go es (Dial (Left  body)   mu) =
    --  go (es ++ [selem "Dial" $ go [] body]) mu
    go es Hangup = es

main :: IO ()
main = do
  lines <- runX $ toXml example >>> writeDocumentToString []
  mapM_ putStrLn lines
