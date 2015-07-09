{-#LANGUAGE DataKinds #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE TypeOperators #-}

module Text.XML.Twiml
  ( response
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

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Types as X
 
response :: IxFree VoiceTwimlF i Void -> VoiceTwiml
response = VoiceTwiml

say :: (Functor1 f, SayF '[Say] :<: f '[Say]) => String -> SayAttributes -> IxFree f '[Say] ()
say a b = iliftF . inj $ SayF a b ()

play :: (Functor1 f, PlayF '[Play] :<: f '[Play]) => URL -> PlayAttributes -> IxFree f '[Play] ()
play a b = iliftF . inj $ PlayF (pure a) b ()

play' :: (Functor1 f, PlayF '[Play] :<: f '[Play]) => Maybe URL -> PlayAttributes -> IxFree f '[Play] ()
play' a b = iliftF . inj $ PlayF a b ()

gather :: (Functor1 f, Nest i In Gather, GatherF '[Gather] :<: f '[Gather]) => GatherAttributes -> IxFree VoiceTwimlF i Void -> IxFree f '[Gather] ()
gather a b = iliftF . inj $ GatherF a b ()

record :: (Functor1 f, RecordF '[Record] :<: f '[Record]) => RecordAttributes -> IxFree f '[Record] ()
record a = iliftF . inj $ RecordF a ()

sms :: (Functor1 f, SmsF '[Sms] :<: f '[Sms]) => String -> SmsAttributes -> IxFree f '[Sms] ()
sms a b = iliftF . inj $ SmsF a b ()

dial :: (Functor1 f, DialF '[Dial] :<: f '[Dial]) => String -> DialAttributes -> IxFree f '[Dial] ()
dial a b = iliftF . inj $ DialF (pure a) b ()

dial' :: (Functor1 f, DialF '[Dial] :<: f '[Dial]) => Either DialNoun String -> DialAttributes -> IxFree f '[Dial] ()
dial' a b = iliftF . inj $ DialF a b ()

enqueue :: (Functor1 f, EnqueueF '[Enqueue] :<: f '[Enqueue]) => String -> EnqueueAttributes -> IxFree f '[Enqueue] ()
enqueue a b = iliftF . inj $ EnqueueF a b ()

leave :: (Functor1 f, LeaveF '[Leave] :<: f '[Leave]) => IxFree f '[Leave] a
leave = iliftF . inj $ LeaveF

hangup :: (Functor1 f, HangupF '[Hangup] :<: f '[Hangup]) => IxFree f '[Hangup] a
hangup = iliftF . inj $ HangupF

redirect :: (Functor1 f, RedirectF '[Redirect] :<: f '[Redirect]) => URL -> RedirectAttributes -> IxFree f '[Redirect] a
redirect a b = iliftF . inj $ RedirectF a b

reject :: (Functor1 f, RejectF '[Reject] :<: f '[Reject]) => RejectAttributes -> IxFree f '[Reject] a
reject a = iliftF . inj $ RejectF a

pause :: (Functor1 f, PauseF '[Pause] :<: f '[Pause]) => PauseAttributes -> IxFree f '[Pause] ()
pause a = iliftF . inj $ PauseF a ()

end :: (Functor1 f, EndF '[End] :<: f '[End]) => IxFree f '[End] a
end = iliftF $ inj EndF
