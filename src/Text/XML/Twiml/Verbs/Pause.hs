{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Pause
  ( -- * @\<Pause\>@
    Pause
  , pause
  , pause'
    -- * Attribute Lenses
  , length'
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Verbs.End (End)
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

newtype Pause    p = Pause    { fromPause    :: Twiml' p }
instance                    Twiml p (Pause    p) where toTwiml' = fromPause

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

length' :: Lens (Pause p) (Pause p) (Maybe Natural) Natural
length' = lens (^. pauseAttributes . to' pauseLength)
  (\t v -> over pauseAttributes (flip setPauseLength v) t)
