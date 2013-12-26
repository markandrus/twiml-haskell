{-#LANGUAGE EmptyDataDecls #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE Rank2Types #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE UndecidableInstances #-}

module Text.XML.Twiml.Internal where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Internal.Lens
import Text.XML.Twiml.Internal.Foldable

import Data.Maybe (catMaybes, fromMaybe)
import Text.XML.Light


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
            -> Maybe URL
            -> a
            -> TwimlF p a
  GatherF   :: NotGatherNoun p
            => GatherAttributes
            -> Twiml' GatherNoun
            -> a
            -> TwimlF p a
  RecordF   :: NotGatherNoun p
            => RecordAttributes
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
  fmap f (RecordF   a b)   = RecordF   a   $ f b
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

instance Show (Twiml' p) where
  show twiml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ (ppElement . unode "Response" $ toXML twiml) ++ "\n"

(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}

string :: String -> Content
string str = Text $ CData CDataText str Nothing

showBool :: Bool -> String
showBool True = "true"
showBool False = "false"

toXML :: Twiml' p -> [Element]
toXML = cata go where
  go EndF = []
  go (SayF a b c) = unode "Say" (string b) & add_attrs (catMaybes
    [ fmap ((Attr $ unqual "voice") . showVoice) $ sayVoice a
    , fmap ((Attr $ unqual "loop") . show) $ sayLoop a
    , fmap (Attr $ unqual "language") $ (sayVoice a >>= showLang)
    ]) : c
  go (PlayF a b c) = (fromMaybe (unode "Play" ()) (fmap (unode "Play" . string . show) b))
      & add_attrs (catMaybes
    [ fmap ((Attr $ unqual "loop") . show) $ playLoop a
    , fmap ((Attr $ unqual "digits") . concatMap show) $ playDigits' a
    ]) : c
  go (GatherF a b c) = unode "Gather" (toXML b) & add_attrs (catMaybes
    [ fmap ((Attr $ unqual "action") . show) $ gatherAction a
    , fmap ((Attr $ unqual "method") . show) $ gatherMethod a
    , fmap ((Attr $ unqual "timeout") . show) $ gatherTimeout a
    , fmap ((Attr $ unqual "finishOnKey") . show) $ gatherFinishOnKey a
    , fmap ((Attr $ unqual "numDigits") . show) $ gatherNumDigits a
    ]) : c
  go (RecordF a b) = unode "Record" () & add_attrs (catMaybes
    [ fmap ((Attr $ unqual "action") . show) $ recordAction a
    , fmap ((Attr $ unqual "method") . show) $ recordMethod a
    , fmap ((Attr $ unqual "timeout") . show) $ recordTimeout a
    , fmap ((Attr $ unqual "finishOnKey") . show) $ recordFinishOnKey a
    , fmap ((Attr $ unqual "maxLength") . show) $ recordMaxLength a
    , fmap ((Attr $ unqual "transcribe") . showBool) $ recordTranscribe a
    , fmap ((Attr $ unqual "transcribeCallback") . show) $ recordTranscribeCallback a
    , fmap ((Attr $ unqual "playBeep") . show) $ recordPlayBeep a
    ]) : b
  go (SmsF a b c) = unode "Sms" (string b) & add_attrs (catMaybes
    [ fmap ((Attr $ unqual "to") . show) $ smsTo a
    , fmap ((Attr $ unqual "from") . show) $ smsFrom a
    , fmap ((Attr $ unqual "action") . show) $ smsAction a
    , fmap ((Attr $ unqual "method") . show) $ smsMethod a
    , fmap ((Attr $ unqual "statusCallback") . show) $ smsStatusCallback a
    ]) : c
  go (DialF a b c) = unode "Dial" () & add_attrs (catMaybes
    [ fmap ((Attr $ unqual "action") . show) $ dialAction a
    , fmap ((Attr $ unqual "method") . show) $ dialMethod a
    , fmap ((Attr $ unqual "timeout") . show) $ dialTimeout a
    , fmap ((Attr $ unqual "hangupOnStar") . showBool) $ dialHangupOnStar a
    , fmap ((Attr $ unqual "timeLimit") . show) $ dialTimeLimit a
    , fmap (Attr $ unqual "callerId") $ dialCallerId a
    , fmap ((Attr $ unqual "record") . showBool) $ dialRecord a
    ]) : c
  go (EnqueueF a b c) = unode "Enqueue" (string b) & add_attrs (catMaybes
    [ fmap ((Attr $ unqual "action") . show) $ enqueueAction a
    , fmap ((Attr $ unqual "method") . show) $ enqueueMethod a
    , fmap ((Attr $ unqual "waitUrl") . show) $ enqueueWaitURL a
    , fmap ((Attr $ unqual "waitUrlMethod") . show) $ enqueueWaitURLMethod a
    ]) : c
  go LeaveF = [unode "Leave" ()]
  go HangupF = [unode "Hangup" ()]
  go (RedirectF a b) = [unode "Redirect" (string $ getURL b) & add_attrs (catMaybes
    [ fmap ((Attr $ unqual "method") . show) $ redirectMethod a
    ])]
  go (RejectF a) = [unode "Reject" () & add_attrs (catMaybes
    [ fmap ((Attr $ unqual "reason") . show) $ rejectReason a
    ])]
  go (PauseF a b) = unode "Pause" () & add_attrs (catMaybes
    [ fmap ((Attr $ unqual "length") . show) $ pauseLength a
    ]) : b

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

number :: String -> Maybe DialNoun
number = Just . Number defaultNumberAttributes

-- This constraint lets us enforce TwiML nesting rules.

class TypeEq x GatherNoun No => NotGatherNoun x

instance TypeEq x GatherNoun No => NotGatherNoun x
