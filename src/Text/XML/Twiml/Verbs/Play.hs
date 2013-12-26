{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Play where

import Text.XML.Twiml.Internal (Fix(..), Lang(..), LangAlice(..), PlayAttributes(..), setPlayDigits, defaultPlayAttributes, Twiml(..), Twiml', TwimlF(..), URL(..), PlayDigit(..))
import Text.XML.Twiml.Internal.Lens ((^.), Lens, Lens', lens, over, to')

newtype Play p = Play { fromPlay :: Twiml' p }
instance Twiml p (Play p) where toTwiml' = fromPlay

play :: Twiml p t => Maybe URL -> t -> Play p
play = play' defaultPlayAttributes

play' :: Twiml p t => PlayAttributes -> Maybe URL -> t -> Play p
play' attrs n = Play . Fix . PlayF attrs n . toTwiml'

playAttributes :: Lens' (Play p) PlayAttributes
playAttributes = lens
  (\(Play (Fix (PlayF attributes _ _))) -> attributes)
  (\(Play (Fix (PlayF _          n a)))    attributes ->
     Play (Fix (PlayF attributes n a)))

digits :: Lens (Play p) (Play p) (Maybe [PlayDigit]) [PlayDigit]
digits = lens (^. playAttributes . to' playDigits')
  (\t v -> over playAttributes (flip setPlayDigits v) t)
