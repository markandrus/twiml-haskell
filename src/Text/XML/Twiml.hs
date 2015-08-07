{-#LANGUAGE ConstraintKinds #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE TypeFamilies #-}

module Text.XML.Twiml
  ( Base
  , IsTwimlLike
  , TwimlLike
  , TwimlLike'
  , MessagingTwiml(..)
  , MessagingTwimlF(..)
  , VoiceTwiml(..)
  , VoiceTwimlF(..)
  , response
  , say
  , play
  , play'
  , gather
  , record
  , sms
  , dial
  , dial'
  , enqueue
  , leave
  , hangup
  , redirect
  , reject
  , pause
  , end
  , module X
  ) where

import Data.Void

import Text.XML.Twiml.Internal as X
import Text.XML.Twiml.Lenses as X
import Text.XML.Twiml.Types as X
import Text.XML.Twiml.Internal.Twiml

-- | 'Base' maps the empty data declaration for a TwiML verb to its
-- corresponding base functor.
type family Base d where
  Base Dial = DialF
  Base End = EndF
  Base Enqueue = EnqueueF
  Base Gather = GatherF
  Base Hangup = HangupF
  Base Leave = LeaveF
--  Base Message = MessageF
  Base Pause = PauseF
  Base Play = PlayF
  Base Record = RecordF
  Base Redirect = RedirectF
  Base Reject = RejectF
  Base Say = SayF
  Base Sms = SmsF

type IsTwimlLike f i = (Functor1 f, (Base i) '[i] :<: f '[i])

type TwimlLike f i = TwimlLike' f '[i]
 
type TwimlLike' f = IxFree f

response :: IxFree VoiceTwimlF i Void -> VoiceTwiml
response = VoiceTwiml

say :: IsTwimlLike f Say => String -> SayAttributes -> TwimlLike f Say ()
say a b = iliftF . inj $ SayF a b ()

play :: IsTwimlLike f Play => URL -> PlayAttributes -> TwimlLike f Play ()
play a b = iliftF . inj $ PlayF (pure a) b ()

play' :: IsTwimlLike f Play => Maybe URL -> PlayAttributes -> TwimlLike f Play ()
play' a b = iliftF . inj $ PlayF a b ()

-- gather :: (Functor1 f, Nest i In Gather, GatherF '[Gather] :<: f '[Gather]) => GatherAttributes -> IxFree VoiceTwimlF i Void -> IxFree f '[Gather] ()
gather :: (IsTwimlLike f Gather, Nest i In Gather) => GatherAttributes -> TwimlLike' VoiceTwimlF i Void -> TwimlLike f Gather ()
gather a b = iliftF . inj $ GatherF a b ()

record :: IsTwimlLike f Record => RecordAttributes -> TwimlLike f Record ()
record a = iliftF . inj $ RecordF a ()

sms :: IsTwimlLike f Sms => String -> SmsAttributes -> TwimlLike f Sms ()
sms a b = iliftF . inj $ SmsF a b ()

dial :: IsTwimlLike f Dial => String -> DialAttributes -> TwimlLike f Dial ()
dial a b = iliftF . inj $ DialF (pure a) b ()

dial' :: IsTwimlLike f Dial => Either DialNoun String -> DialAttributes -> TwimlLike f Dial ()
dial' a b = iliftF . inj $ DialF a b ()

enqueue :: IsTwimlLike f Enqueue => String -> EnqueueAttributes -> TwimlLike f Enqueue ()
enqueue a b = iliftF . inj $ EnqueueF a b ()

leave :: IsTwimlLike f Leave => TwimlLike f Leave a
leave = iliftF . inj $ LeaveF

hangup :: IsTwimlLike f Hangup => TwimlLike f Hangup a
hangup = iliftF . inj $ HangupF

redirect :: IsTwimlLike f Redirect => URL -> RedirectAttributes -> TwimlLike f Redirect a
redirect a b = iliftF . inj $ RedirectF a b

reject :: IsTwimlLike f Reject => RejectAttributes -> TwimlLike f Reject a
reject a = iliftF . inj $ RejectF a

pause :: IsTwimlLike f Pause => PauseAttributes -> TwimlLike f Pause ()
pause a = iliftF . inj $ PauseF a ()

end :: IsTwimlLike f End => TwimlLike f End a
end = iliftF $ inj EndF
