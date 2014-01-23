{-#LANGUAGE EmptyDataDecls #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE TypeOperators #-}

module Text.XML.Twiml.Internal
  ( TwimlF(..)
  , Twiml(..)
  , Twiml'
  ) where

import Text.XML.Twiml.Types

import Data.Maybe (catMaybes, fromMaybe)
import Text.XML.Light

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
  GatherF   :: ((p :/~ Gather'))
            => GatherAttributes
            -> Twiml' Gather'
            -> a
            -> TwimlF p a
  RecordF   :: (p :/~ Gather')
            => RecordAttributes
            -> a
            -> TwimlF p a
  SmsF      :: (p :/~ Gather')
            => SmsAttributes
            -> String
            -> a
            -> TwimlF p a
  DialF     :: (p :/~ Gather')
            => DialAttributes
            -> Either DialNoun String
            -> a
            -> TwimlF p a
  EnqueueF  :: (p :/~ Gather')
            => EnqueueAttributes
            -> String
            -> a
            -> TwimlF p a
  LeaveF    :: (p :/~ Gather')
            => TwimlF p a
  HangupF   :: (p :/~ Gather')
            => TwimlF p a
  RedirectF :: (p :/~ Gather')
            => RedirectAttributes
            -> URL
            -> TwimlF p a
  RejectF   :: (p :/~ Gather')
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

showVoice :: Voice -> String
showVoice (Man _) = "man"
showVoice (Woman _) = "woman"
showVoice (Alice _) = "alice"

showLang :: Voice -> Maybe String
showLang (Man lang) = fmap show lang 
showLang (Woman lang) = fmap show lang 
showLang (Alice lang) = fmap show lang 

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
    , fmap ((Attr $ unqual "digits") . concatMap show) $ playDigits a
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
  go (RedirectF a b) = [unode "Redirect" (string $ show b) & add_attrs (catMaybes
    [ fmap ((Attr $ unqual "method") . show) $ redirectMethod a
    ])]
  go (RejectF a) = [unode "Reject" () & add_attrs (catMaybes
    [ fmap ((Attr $ unqual "reason") . show) $ rejectReason a
    ])]
  go (PauseF a b) = unode "Pause" () & add_attrs (catMaybes
    [ fmap ((Attr $ unqual "length") . show) $ pauseLength a
    ]) : b

{- Twiml Attributes -}

-- FIXME: Rename `PlayKey`.

number :: String -> Maybe DialNoun
number = Just . Number defaultNumberAttributes

