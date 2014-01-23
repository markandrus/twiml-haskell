{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Play
  ( -- * @\<Play\>@
    Play
    -- ** Constructors
  , play
  , play'
    -- ** Attributes
  , PlayAttributes(..)
  , defaultPlayAttributes
    -- *** Lenses
  , playAttributes
  , digits
  , loop
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

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

setPlayLoop :: PlayAttributes -> Natural -> PlayAttributes
setPlayLoop attrs loop = attrs { playLoop = Just loop }

setPlayDigits :: PlayAttributes -> [Digit] -> PlayAttributes
setPlayDigits attrs digits = attrs { playDigits = Just digits }

digits :: Lens (Play p) (Play p) (Maybe [Digit]) [Digit]
digits = lens (^. playAttributes . to' playDigits)
  (\t v -> over playAttributes (flip setPlayDigits v) t)

instance HasLoop (Play p) where
  loop = lens getLoop setLoop where
    getLoop = (^. playAttributes . to' playLoop)
    setLoop t v = over playAttributes (flip setPlayLoop v) t
