{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Say where

import Text.XML.Twiml.Internal (Fix(..), Lang(..), LangAlice(..), SayAttributes(..), setSayVoice, defaultSayAttributes, Twiml(..), Twiml', TwimlF(..), Voice(..))
import Text.XML.Twiml.Internal.Lens ((^.), Lens, Lens', lens, over, to')

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

voice :: Lens (Say p) (Say p) (Maybe Voice) Voice
voice = lens (^. sayAttributes . to' sayVoice)
  (\t v -> over sayAttributes (flip setSayVoice v) t)
