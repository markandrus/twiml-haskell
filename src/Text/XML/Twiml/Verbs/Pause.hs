{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Pause
  ( -- * @\<Pause\>@
    -- $pause
    Pause
    -- ** Constructors
  , pause
  , pause'
    -- ** Attributes
  , PauseAttributes(..)
  , defaultPauseAttributes
    -- *** Lenses
  , pauseAttributes
  , duration
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Verbs.End (End)
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

{- $pause This example

@
module Example where

import Control.Lens
import Text.XML.Twiml

example
  = respond
  . say \"I will pause 10 seconds starting now!\"
  . (pause \<&\> duration .~ 10)
  . say \"I just paused 10 seconds\"
  $ end
@

produces the following TwiML response:

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response\>
  \<Say\>I will pause 10 seconds starting now!\<\/Say\>
  \<Pause length=\"10\" \/\>
  \<Say\>I just paused 10 seconds\<\/Say\>
\<\/Response\>
@
-}

newtype Pause p = Pause { fromPause :: Twiml' p }
instance Twiml p (Pause p) where toTwiml' = fromPause

pause :: Twiml p t => t -> Pause p
pause = pause' defaultPauseAttributes

pause' :: Twiml p t => PauseAttributes -> t -> Pause p
pause' attrs = Pause . Fix . PauseF attrs . toTwiml'

pauseAttributes :: Lens' (Pause p) PauseAttributes
pauseAttributes = lens
  (\(Pause (Fix (PauseF attributes _))) -> attributes)
  (\(Pause (Fix (PauseF _          a)))    attributes ->
     Pause (Fix (PauseF attributes a)))

setPauseLength :: PauseAttributes -> Natural -> PauseAttributes
setPauseLength attrs length = attrs { pauseLength = Just length }

duration :: Lens (Pause p) (Pause p) (Maybe Natural) Natural
duration = lens (^. pauseAttributes . to' pauseLength)
  (\t v -> over pauseAttributes (flip setPauseLength v) t)
