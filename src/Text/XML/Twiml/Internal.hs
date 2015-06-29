{-#LANGUAGE DataKinds #-}
{-#LANGUAGE EmptyDataDecls #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE UndecidableInstances #-}

module Text.XML.Twiml.Internal
  (
    response
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
  , Twiml(..)
  , Twiml'
  , In
  , Nest
  , TwimlF(..)
  , Say
  , Play
  , Gather
  , Record
  , Sms
  , Dial
  , Enqueue
  , Leave
  , Hangup
  , Redirect
  , Reject
  , Pause
  , End
  ) where

import Text.XML.Twiml.Types
import Text.XML.Light

import Data.Void

response :: Twiml' i Void -> Twiml
response = Response

say :: String -> SayAttributes -> Twiml' '[Say] ()
say a b = iliftF $ SayF a b ()

play :: URL -> PlayAttributes -> Twiml' '[Play] ()
play a b = iliftF $ PlayF (Just a) b ()

play' :: Maybe URL -> PlayAttributes -> Twiml' '[Play] ()
play' a b = iliftF $ PlayF a b ()

gather :: Nest i In Gather
       => GatherAttributes -> Twiml' i Void -> Twiml' '[Gather] ()
gather a b = iliftF $ GatherF a b ()

record :: RecordAttributes -> Twiml' '[Record] ()
record a = iliftF $ RecordF a ()

sms :: String -> SmsAttributes -> Twiml' '[Sms] ()
sms a b = iliftF $ SmsF a b ()

dial :: String -> DialAttributes -> Twiml' '[Dial] ()
dial a b = iliftF $ DialF (Right a) b ()

dial' :: Either DialNoun String -> DialAttributes -> Twiml' '[Dial] ()
dial' a b = iliftF $ DialF a b ()

enqueue :: String -> EnqueueAttributes -> Twiml' '[Enqueue] ()
enqueue a b = iliftF $ EnqueueF a b ()

leave :: Twiml' '[Leave] a
leave = iliftF LeaveF

hangup :: Twiml' '[Hangup] a
hangup = iliftF HangupF

redirect :: URL -> RedirectAttributes -> Twiml' '[Redirect] a
redirect a b = iliftF $ RedirectF a b

reject :: RejectAttributes -> Twiml' '[Reject] a
reject a = iliftF $ RejectF a

pause :: PauseAttributes -> Twiml' '[Pause] ()
pause a = iliftF $ PauseF a ()

end :: Twiml' '[] a
end = iliftF EndF

data Twiml = forall i. Response (Twiml' (i :: [*]) Void)

instance Show Twiml where
  show = showTwiml -- show (Response a) = "Response (" ++ show a ++ ")"

type Twiml' = IxFree TwimlF

data In

type family Nest a i b where
  Nest i In Gather =
    ( Record   ∉ i
    , Gather   ∉ i
    , Sms      ∉ i
    , Dial     ∉ i
    , Enqueue  ∉ i
    , Leave    ∉ i
    , Hangup   ∉ i
    , Redirect ∉ i
    , Reject   ∉ i
    )

data TwimlF i a where
  SayF      :: String
            -> SayAttributes
            -> a
            -> TwimlF '[Say] a
  PlayF     :: Maybe URL
            -> PlayAttributes
            -> a
            -> TwimlF '[Play] a
  GatherF   :: Nest i In Gather
            => GatherAttributes
            -> Twiml' i Void
            -> a
            -> TwimlF '[Gather] a
  RecordF   :: RecordAttributes
            -> a
            -> TwimlF '[Record] a
  SmsF      :: String
            -> SmsAttributes
            -> a
            -> TwimlF '[Sms] a
  DialF     :: Either DialNoun String
            -> DialAttributes
            -> a
            -> TwimlF '[Dial] a
  EnqueueF  :: String
            -> EnqueueAttributes
            -> a
            -> TwimlF '[Enqueue] a
  LeaveF    :: TwimlF '[Leave] a
  HangupF   :: TwimlF '[Hangup] a
  RedirectF :: URL
            -> RedirectAttributes
            -> TwimlF '[Redirect] a
  RejectF   :: RejectAttributes
            -> TwimlF '[Reject] a
  PauseF    :: PauseAttributes
            -> a
            -> TwimlF '[Pause] a
  EndF      :: TwimlF '[] a

instance Functor (TwimlF i) where
  fmap f (SayF      a b c) = SayF      a b $ f c
  fmap f (PlayF     a b c) = PlayF     a b $ f c
  fmap f (GatherF   a b c) = GatherF   a b $ f c
  fmap f (RecordF   a b)   = RecordF   a   $ f b
  fmap f (SmsF      a b c) = SmsF      a b $ f c
  fmap f (DialF     a b c) = DialF     a b $ f c
  fmap f (EnqueueF  a b c) = EnqueueF  a b $ f c
  fmap _  LeaveF           = LeaveF
  fmap _  HangupF          = HangupF
  fmap _ (RedirectF a b)   = RedirectF a b
  fmap _ (RejectF   a)     = RejectF   a
  fmap f (PauseF    a b)   = PauseF    a   $ f b
  fmap _  EndF             = EndF

instance IxFunctor TwimlF where
  imap = fmap

instance Show a => Show (TwimlF i a) where
  show (SayF      a b c) = "SayF ("      ++ show a ++ ") (" ++  show b ++ ") (" ++ show c ++ ")"
  show (PlayF     a b c) = "PlayF ("     ++ show a ++ ") (" ++  show b ++ ") (" ++ show c ++ ")"
  show (GatherF   a b c) = "GatherF ("   ++ show a ++ ") (" ++ ishow b ++ ") (" ++ show c ++ ")"
  show (RecordF   a b)   = "RecordF ("   ++ show a ++ ") (" ++  show b ++ ")"
  show (SmsF      a b c) = "SmsF ("      ++ show a ++ ") (" ++  show b ++ ") (" ++ show c ++ ")"
  show (DialF     a b c) = "DialF ("     ++ show a ++ ") (" ++  show b ++ ") (" ++ show c ++ ")"
  show (EnqueueF  a b c) = "EnqueueF ("  ++ show a ++ ") (" ++  show b ++ ") (" ++ show c ++ ")"
  show  LeaveF           = "LeaveF"
  show  HangupF          = "HangupF"
  show (RedirectF a b)   = "RedirectF (" ++ show a ++ ") (" ++  show b ++ ")"
  show (RejectF   a)     = "RejectF ("   ++ show a ++ ")"
  show (PauseF    a b)   = "PauseF ("    ++ show a ++ ") (" ++  show b
  show  EndF             = "EndF"

instance IxShow TwimlF where
  ishow = show

data Say
data Play
data Gather
data Record
data Sms
data Dial
data Enqueue
data Leave
data Hangup
data Redirect
data Reject
data Pause
data End

{-
type instance Base (Fix (TwimlF p)) = TwimlF p

instance Text.XML.Twiml.Types.Foldable (Fix (TwimlF p)) where
  project = unFix

instance Show a => Show (Twiml' i a) where
  show twiml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ (ppElement . unode "Response" $ toXML twiml) ++ "\n"
-}

showTwiml :: Twiml -> String
showTwiml twiml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ ppElement (toElement twiml) ++ "\n"

instance ToElement Twiml where
  toElement (Response twiml) = unode "Response" $ toXML twiml

instance ToXML (Twiml' i Void) where
  toXML (IxFree twiml) = toXML twiml
  toXML (IxPure _) = []

instance ToXML (TwimlF i (Twiml' j Void)) where
  toXML (SayF      a         attrs b) =  makeElement "Say"      (strToContent a)  (toAttrs attrs) : toXML b
  toXML (PlayF     (Just a)  attrs b) =  makeElement "Play"     (urlToContent a)  (toAttrs attrs) : toXML b
  toXML (PlayF     _         attrs b) =  makeElement "Play"     ()                (toAttrs attrs) : toXML b
  toXML (GatherF   attrs     a     b) =  makeElement "Gather"   (toXML        a)  (toAttrs attrs) : toXML b
  toXML (RecordF             attrs a) =  makeElement "Record"   ()                (toAttrs attrs) : toXML a
  toXML (SmsF      a         attrs b) =  makeElement "Sms"      (strToContent a)  (toAttrs attrs) : toXML b
  toXML (DialF     (Left  a) attrs b) =  makeElement "Dial"     (toElement    a)  (toAttrs attrs) : toXML b
  toXML (DialF     (Right a) attrs b) =  makeElement "Dial"     (strToContent a)  (toAttrs attrs) : toXML b
  toXML (EnqueueF  a         attrs b) =  makeElement "Enqueue"  (strToContent a)  (toAttrs attrs) : toXML b
  toXML  LeaveF                       = [makeElement "Leave"    ()               []]
  toXML  HangupF                      = [makeElement "Hangup"   ()               []]
  toXML (RedirectF a         attrs)   = [makeElement "Redirect" (urlToContent a) $ toAttrs attrs]
  toXML (RejectF             attrs)   = [makeElement "Reject"   ()               $ toAttrs attrs]
  toXML (PauseF              attrs a) =  makeElement "Pause"    ()                (toAttrs attrs) : toXML a
  toXML  EndF                         = []
