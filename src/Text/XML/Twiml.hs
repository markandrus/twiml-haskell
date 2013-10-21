{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE RankNTypes #-}

module Text.XML.Twiml
  ( Twiml
  -- * Types
  -- ** @\<Response\>@
  , Response
  , respond
  -- ** URL
  , URL
  -- ** Method
  , Method(..)
  -- ** Keys and Digits
  , Key(..)
  , PlayDigit(..)
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
  , digits
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
  , DialNoun(..)
  , dial
  , dial'
  , hangupOnStar
  , timeLimit
  , callerId
  , recordDial
  , dialAttributes
  -- *** @\<Number\>@
  , NumberAttributes(..)
  -- *** @\<Sip\>@
  , SipAttributes(..)
  , Transport(..)
  -- *** @\<Client\>@
  , ClientAttributes(..)
  -- *** @\<Conference\>@
  , ConferenceAttributes(..)
  , ConferenceBeep(..)
  -- *** @\<Queue\>@
  , QueueAttributes(..)
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

import Text.XML.Twiml.Internal

import Unsafe.Coerce

{- Basic Lens Functionality -}

-- The following section extracts a number of definitions required to get
-- lenses, as defined in the lens package, working, without relying on the lens
-- package itself. If this turns out to be a bad idea, please email me.

-- The following definitions were extracted from the lens package.

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

{- Twiml Response -}

newtype Response = Response { fromResponse :: Twiml' Response }

instance Twiml Response Response where
  toTwiml' = fromResponse

respond :: Twiml Response t => t -> Response
respond = Response . toTwiml'

{- Verb Newtypes -}

newtype End      p = End      { fromEnd      :: Twiml' p }
newtype Say      p = Say      { fromSay      :: Twiml' p }
newtype Play     p = Play     { fromPlay     :: Twiml' p }
newtype Gather   p = Gather   { fromGather   :: Twiml' p }
newtype Record   p = Record   { fromRecord   :: Twiml' p }
newtype Sms      p = Sms      { fromSms      :: Twiml' p }
newtype Dial     p = Dial     { fromDial     :: Twiml' p }
newtype Enqueue  p = Enqueue  { fromEnqueue  :: Twiml' p }
newtype Leave    p = Leave    { fromLeave    :: Twiml' p }
newtype Hangup   p = Hangup   { fromHangup   :: Twiml' p }
newtype Redirect p = Redirect { fromRedirect :: Twiml' p }
newtype Reject   p = Reject   { fromReject   :: Twiml' p }
newtype Pause    p = Pause    { fromPause    :: Twiml' p }

instance                    Twiml p (End      p) where toTwiml' = fromEnd
instance                    Twiml p (Say      p) where toTwiml' = fromSay
instance                    Twiml p (Play     p) where toTwiml' = fromPlay
instance NotGatherNoun p => Twiml p (Gather   p) where toTwiml' = fromGather
instance NotGatherNoun p => Twiml p (Record   p) where toTwiml' = fromRecord
instance NotGatherNoun p => Twiml p (Sms      p) where toTwiml' = fromSms
instance NotGatherNoun p => Twiml p (Dial     p) where toTwiml' = fromDial
instance NotGatherNoun p => Twiml p (Enqueue  p) where toTwiml' = fromEnqueue
instance NotGatherNoun p => Twiml p (Leave    p) where toTwiml' = fromLeave
instance NotGatherNoun p => Twiml p (Hangup   p) where toTwiml' = fromHangup
instance NotGatherNoun p => Twiml p (Redirect p) where toTwiml' = fromRedirect
instance NotGatherNoun p => Twiml p (Reject   p) where toTwiml' = fromReject
instance                    Twiml p (Pause    p) where toTwiml' = fromPause

{- Verb Constructors -}

end :: End p
end = End . Fix $ EndF

say :: Twiml p t => String -> t -> Say p
say = say' defaultSayAttributes

say' :: Twiml p t => SayAttributes -> String -> t -> Say p
say' a b = Say . Fix . SayF a b . toTwiml'

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

play :: Twiml p t => URL -> t -> Play p
play = play' defaultPlayAttributes

play' :: Twiml p t => PlayAttributes -> URL -> t -> Play p
play' attrs n = Play . Fix . PlayF attrs n . toTwiml'

gather :: (Twiml GatherNoun n, Twiml p t, NotGatherNoun p) => n -> t -> Gather p
gather = gather' defaultGatherAttributes

gather' :: (Twiml GatherNoun n, Twiml p t, NotGatherNoun p)
        => GatherAttributes -> n -> t -> Gather p
gather' attrs n
  = Gather . Fix . GatherF attrs (toTwiml' n) . toTwiml'

record :: (Twiml p t, NotGatherNoun p) => URL -> t -> Record p
record = record' defaultRecordAttributes

record' :: (Twiml p t, NotGatherNoun p)
        => RecordAttributes -> URL -> t -> Record p
record' attrs url
  = Record . Fix . RecordF attrs url . toTwiml'

sms :: (Twiml p t, NotGatherNoun p) => String -> t -> Sms p
sms = sms' defaultSmsAttributes

sms' :: (Twiml p t, NotGatherNoun p) => SmsAttributes -> String -> t -> Sms p
sms' attrs n = Sms . Fix . SmsF attrs n . toTwiml'

dial :: (Twiml p t, NotGatherNoun p) => Either DialNoun String -> t -> Dial p
dial = dial' defaultDialAttributes

dial' :: (Twiml p t, NotGatherNoun p)
      => DialAttributes -> Either DialNoun String -> t -> Dial p
dial' attrs n = Dial . Fix . DialF attrs n . toTwiml'

enqueue :: (Twiml p t, NotGatherNoun p) => String -> t -> Enqueue p
enqueue name = Enqueue . Fix . EnqueueF defaultEnqueueAttributes name . toTwiml'

leave :: NotGatherNoun p => End p -> Leave p
leave = const . Leave $ Fix LeaveF

hangup :: NotGatherNoun p => End p -> Hangup p
hangup = const . Hangup $ Fix HangupF

redirect :: NotGatherNoun p => URL -> End p -> Redirect p
redirect = redirect' defaultRedirectAttributes

redirect' :: NotGatherNoun p => RedirectAttributes -> URL -> End p -> Redirect p
redirect' attrs url = const . Redirect . Fix $ RedirectF attrs url

reject :: NotGatherNoun p => End p -> Reject p
reject = reject' defaultRejectAttributes

reject' :: NotGatherNoun p => RejectAttributes -> End p -> Reject p
reject' attrs = const . Reject . Fix $ RejectF attrs

pause :: Twiml p t => t -> Pause p
pause = pause' defaultPauseAttributes

pause' :: Twiml p t => PauseAttributes -> t -> Pause p
pause' attrs = Pause . Fix . PauseF attrs . toTwiml'

{- Verb-specific Lenses -}

sayAttributes :: Lens' (Say p) SayAttributes
sayAttributes = lens
  (\(Say (Fix (SayF attributes _ _))) -> attributes)
  (\(Say (Fix (SayF _          n a)))    attributes ->
     Say (Fix (SayF attributes n a)))

voice :: Lens (Say p) (Say p) (Maybe Voice) Voice
voice = lens (^. sayAttributes . to' sayVoice)
  (\t v -> over sayAttributes (flip setSayVoice v) t)

playAttributes :: Lens' (Play p) PlayAttributes
playAttributes = lens
  (\(Play (Fix (PlayF attributes _ _))) -> attributes)
  (\(Play (Fix (PlayF _          n a)))    attributes ->
     Play (Fix (PlayF attributes n a)))

digits :: Lens (Play p) (Play p) (Maybe [PlayDigit]) [PlayDigit]
digits = lens (^. playAttributes . to' playDigits')
  (\t v -> over playAttributes (flip setPlayDigits v) t)

gatherAttributes :: Lens' (Gather p) GatherAttributes
gatherAttributes = lens
  (\(Gather (Fix (GatherF attributes _ _))) -> attributes)
  (\(Gather (Fix (GatherF _          n a)))    attributes ->
     Gather (Fix (GatherF attributes n a)))

numDigits :: Lens (Gather p) (Gather p) (Maybe Natural) Natural
numDigits = lens (^. gatherAttributes . to' gatherNumDigits)
  (\t v -> over gatherAttributes (flip setGatherNumDigits v) t)

recordAttributes :: Lens' (Record p) RecordAttributes
recordAttributes = lens
  (\(Record (Fix (RecordF attributes _ _))) -> attributes)
  (\(Record (Fix (RecordF _          n a)))    attributes ->
     Record (Fix (RecordF attributes n a)))

maxLength :: Lens (Record p) (Record p) (Maybe Natural) Natural
maxLength = lens (^. recordAttributes . to' recordMaxLength)
  (\t v -> over recordAttributes (flip setRecordMaxLength v) t)

transcribe :: Lens (Record p) (Record p) (Maybe Bool) Bool
transcribe = lens (^. recordAttributes . to' recordTranscribe)
  (\t v -> over recordAttributes (flip setRecordTranscribe v) t)

transcribeCallback :: Lens (Record p) (Record p) (Maybe URL) URL
transcribeCallback = lens (^. recordAttributes . to' recordTranscribeCallback)
  (\t v -> over recordAttributes (flip setRecordTranscribeCallback v) t)

playBeep :: Lens (Record p) (Record p) (Maybe Bool) Bool
playBeep = lens (^. recordAttributes . to' recordPlayBeep)
  (\t v -> over recordAttributes (flip setRecordPlayBeep v) t)

smsAttributes :: Lens' (Sms p) SmsAttributes
smsAttributes = lens
  (\(Sms (Fix (SmsF attributes _ _))) -> attributes)
  (\(Sms (Fix (SmsF _          n a)))    attributes ->
     Sms (Fix (SmsF attributes n a)))

to :: Lens (Sms p) (Sms p) (Maybe String) String
to = lens (^. smsAttributes . to' smsTo)
  (\t v -> over smsAttributes (flip setSmsTo v) t)

from :: Lens (Sms p) (Sms p) (Maybe String) String
from = lens (^. smsAttributes . to' smsFrom)
  (\t v -> over smsAttributes (flip setSmsFrom v) t)

statusCallback :: Lens (Sms p) (Sms p) (Maybe URL) URL
statusCallback = lens (^. smsAttributes . to' smsStatusCallback)
  (\t v -> over smsAttributes (flip setSmsStatusCallback v) t)

dialAttributes :: Lens' (Dial p) DialAttributes
dialAttributes = lens
  (\(Dial (Fix (DialF attributes _ _))) -> attributes)
  (\(Dial (Fix (DialF _          n a)))    attributes ->
     Dial (Fix (DialF attributes n a)))

hangupOnStar :: Lens (Dial p) (Dial p) (Maybe Bool) Bool
hangupOnStar = lens (^. dialAttributes . to' dialHangupOnStar)
  (\t v -> over dialAttributes (flip setDialHangupOnStar v) t)

timeLimit :: Lens (Dial p) (Dial p) (Maybe Natural) Natural
timeLimit = lens (^. dialAttributes . to' dialTimeLimit)
  (\t v -> over dialAttributes (flip setDialTimeLimit v) t)

callerId :: Lens (Dial p) (Dial p) (Maybe String) String
callerId = lens (^. dialAttributes . to' dialCallerId)
  (\t v -> over dialAttributes (flip setDialCallerId v) t)

recordDial :: Lens (Dial p) (Dial p) (Maybe Bool) Bool
recordDial = lens (^. dialAttributes . to' dialRecord)
  (\t v -> over dialAttributes (flip setDialRecord v) t)

redirectAttributes :: Lens' (Redirect p) RedirectAttributes
redirectAttributes = lens
  (\(Redirect (Fix (RedirectF attributes _))) -> attributes)
  (\(Redirect (Fix (RedirectF _          n)))    attributes ->
     Redirect (Fix (RedirectF attributes n)))

rejectAttributes :: Lens' (Reject p) RejectAttributes
rejectAttributes = lens
  (\(Reject (Fix (RejectF attributes))) -> attributes)
  (\(Reject (Fix (RejectF _         )))    attributes ->
     Reject (Fix (RejectF attributes)))

reason :: Lens (Reject p) (Reject p) (Maybe Reason) Reason
reason = lens (^. rejectAttributes . to' rejectReason)
  (\t v -> over rejectAttributes (flip setRejectReason v) t)

pauseAttributes :: Lens' (Pause p) PauseAttributes
pauseAttributes = lens
  (\(Pause (Fix (PauseF attributes _))) -> attributes)
  (\(Pause (Fix (PauseF _          a)))    attributes ->
     Pause (Fix (PauseF attributes a)))

length :: Lens (Pause p) (Pause p) (Maybe Natural) Natural
length = lens (^. pauseAttributes . to' pauseLength)
  (\t v -> over pauseAttributes (flip setPauseLength v) t)

{- Generic Lenses -}

class HasLoop t where
  getLoop :: t -> Maybe Natural
  setLoop :: t -> Natural -> t

instance HasLoop (Say p) where
  getLoop = (^. sayAttributes . to' sayLoop)
  setLoop t v = over sayAttributes (flip setSayLoop v) t

instance HasLoop (Play p) where
  getLoop = (^. playAttributes . to' playLoop)
  setLoop t v = over playAttributes (flip setPlayLoop v) t

loop :: HasLoop t => Lens t t (Maybe Natural) Natural
loop = lens getLoop setLoop

class HasAction t where
  getAction :: t -> Maybe URL
  setAction :: t -> URL -> t

instance HasAction (Gather p) where
  getAction = (^. gatherAttributes . to' gatherAction)
  setAction t v = over gatherAttributes (flip setGatherAction v) t

instance HasAction (Record p) where
  getAction = (^. recordAttributes . to' recordAction)
  setAction t v = over recordAttributes (flip setRecordAction v) t

instance HasAction (Sms p) where
  getAction = (^. smsAttributes . to' smsAction)
  setAction t v = over smsAttributes (flip setSmsAction v) t

instance HasAction (Dial p) where
  getAction = (^. dialAttributes . to' dialAction)
  setAction t v = over dialAttributes (flip setDialAction v) t

action :: HasAction t => Lens t t (Maybe URL) URL
action = lens getAction setAction

class HasMethod t where
  getMethod :: t -> Maybe Method
  setMethod :: t -> Method -> t

instance HasMethod (Gather p) where
  getMethod = (^. gatherAttributes . to' gatherMethod)
  setMethod t v = over gatherAttributes (flip setGatherMethod v) t

instance HasMethod (Record p) where
  getMethod = (^. recordAttributes . to' recordMethod)
  setMethod t v = over recordAttributes (flip setRecordMethod v) t

instance HasMethod (Sms p) where
  getMethod = (^. smsAttributes . to' smsMethod)
  setMethod t v = over smsAttributes (flip setSmsMethod v) t

instance HasMethod (Dial p) where
  getMethod = (^. dialAttributes . to' dialMethod)
  setMethod t v = over dialAttributes (flip setDialMethod v) t

instance HasMethod (Redirect p) where
  getMethod = (^. redirectAttributes . to' redirectMethod)
  setMethod t v = over redirectAttributes (flip setRedirectMethod v) t

method :: HasMethod t => Lens t t (Maybe Method) Method
method = lens getMethod setMethod

class HasTimeout t where
  getTimeout :: t -> Maybe Natural
  setTimeout :: t -> Natural -> t

instance HasTimeout (Gather p) where
  getTimeout = (^. gatherAttributes . to' gatherTimeout)
  setTimeout t v = over gatherAttributes (flip setGatherTimeout v) t

instance HasTimeout (Record p) where
  getTimeout = (^. recordAttributes . to' recordTimeout)
  setTimeout t v = over recordAttributes (flip setRecordTimeout v) t

instance HasTimeout (Dial p) where
  getTimeout = (^. dialAttributes . to' dialTimeout)
  setTimeout t v = over dialAttributes (flip setDialTimeout v) t

timeout :: HasTimeout t => Lens t t (Maybe Natural) Natural
timeout = lens getTimeout setTimeout

class HasFinishOnKey t where
  getFinishOnKey :: t -> Maybe Key
  setFinishOnKey :: t -> Key -> t

instance HasFinishOnKey (Gather p) where
  getFinishOnKey = (^. gatherAttributes . to' gatherFinishOnKey)
  setFinishOnKey t v = over gatherAttributes (flip setGatherFinishOnKey v) t

instance HasFinishOnKey (Record p) where
  getFinishOnKey = (^. recordAttributes . to' recordFinishOnKey)
  setFinishOnKey t v = over recordAttributes (flip setRecordFinishOnKey v) t

finishOnKey :: HasFinishOnKey t => Lens t t (Maybe Key) Key
finishOnKey = lens getFinishOnKey setFinishOnKey
