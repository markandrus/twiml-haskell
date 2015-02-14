{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Say
  ( -- * @\<Say\>@
    -- $say
    Say
    -- ** Constructors
  , say
  , say'
  , sayMan
  , sayMan'
  , sayWoman
  , sayWoman'
  , sayAlice
  , sayAlice'
    -- ** Attributes
  , SayAttributes(..)
  , defaultSayAttributes
    -- *** Lenses
  , sayAttributes
  , voice
  , loop
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

{- $say This example

@
module Example where

import Text.XML.Twiml

example
  = respond
  . sayWoman' French \"Chapeau\"
  $ end
@

produces the following TwiML response:

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response\>
  \<Say voice=\"woman\" language=\"fr\"\>Chapeau\<\/Say\>
\<\/Response\>
@
-}

newtype Say p = Say { fromSay :: Twiml' p }
instance Twiml p (Say p) where toTwiml' = fromSay

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

sayAttributes :: Lens' (Say p) SayAttributes
sayAttributes = lens
  (\(Say (Fix (SayF attributes _ _))) -> attributes)
  (\(Say (Fix (SayF _          n a)))    attributes ->
     Say (Fix (SayF attributes n a)))

{-

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

-}

setSayVoice :: SayAttributes -> Voice -> SayAttributes
setSayVoice attrs voice = attrs { sayVoice = Just voice }

setSayLoop :: SayAttributes -> Natural -> SayAttributes
setSayLoop attrs loop = attrs { sayLoop = Just loop }

voice :: Lens (Say p) (Say p) (Maybe Voice) Voice
voice = lens (^. sayAttributes . to' sayVoice)
  (\t v -> over sayAttributes (`setSayVoice` v) t)

instance HasLoop (Say p) where
  loop = lens getLoop setLoop where
    getLoop = (^. sayAttributes . to' sayLoop)
    setLoop t v = over sayAttributes (`setSayLoop` v) t
