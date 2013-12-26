{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE RankNTypes #-}

module Text.XML.Twiml.Verbs
  ( module Text.XML.Twiml.Verbs.End
  , module Text.XML.Twiml.Verbs.Say
  , module Text.XML.Twiml.Verbs.Play
  , module Text.XML.Twiml.Verbs.Gather
  , module Text.XML.Twiml.Verbs.Record
  , module Text.XML.Twiml.Verbs.Sms
  , module Text.XML.Twiml.Verbs.Dial
  , module Text.XML.Twiml.Verbs.Enqueue
  , module Text.XML.Twiml.Verbs.Leave
  , module Text.XML.Twiml.Verbs.Hangup
  , module Text.XML.Twiml.Verbs.Redirect
  , module Text.XML.Twiml.Verbs.Reject
  , module Text.XML.Twiml.Verbs.Pause
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

import Text.XML.Twiml.Verbs.End

import Text.XML.Twiml.Verbs.Say
import Text.XML.Twiml.Verbs.Play
import Text.XML.Twiml.Verbs.Gather
import Text.XML.Twiml.Verbs.Record
import Text.XML.Twiml.Verbs.Sms
import Text.XML.Twiml.Verbs.Dial

import Text.XML.Twiml.Verbs.Enqueue
import Text.XML.Twiml.Verbs.Leave
import Text.XML.Twiml.Verbs.Hangup
import Text.XML.Twiml.Verbs.Redirect
import Text.XML.Twiml.Verbs.Reject
import Text.XML.Twiml.Verbs.Pause

import Text.XML.Twiml.Internal (Twiml, Natural, URL(..), Method(..), Key(..), setSayLoop, sayLoop, setPlayLoop, playLoop, gatherAction, setGatherAction, recordAction, setRecordAction, smsAction, setSmsAction, dialAction, setDialAction, gatherMethod, setGatherMethod, recordMethod, setRecordMethod, smsMethod, setSmsMethod, dialMethod, setDialMethod, redirectMethod, setRedirectMethod, gatherTimeout, setGatherTimeout, recordTimeout, setRecordTimeout, dialTimeout, setDialTimeout, gatherFinishOnKey, setGatherFinishOnKey, recordFinishOnKey, setRecordFinishOnKey)
import Text.XML.Twiml.Internal.Lens ((^.), Lens, lens, over, to')

import Unsafe.Coerce (unsafeCoerce)

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

instance forall p t. HasAction (t -> Gather p) where
  getAction f = (^. gatherAttributes . to' gatherAction) (f $ unsafeCoerce end)
  setAction f v = fmap (over gatherAttributes (flip setGatherAction v)) f

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

instance forall p t. Twiml p t => HasMethod (t -> Gather p) where
  getMethod f = (^. gatherAttributes . to' gatherMethod) (f $ unsafeCoerce end)
  setMethod f v = fmap (over gatherAttributes (flip setGatherMethod v)) f

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
