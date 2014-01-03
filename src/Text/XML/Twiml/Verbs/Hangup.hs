{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Hangup
  ( -- * @\<Hangup\>@
    Hangup
  , hangup
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Verbs.End (End)
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

newtype Hangup p = Hangup { fromHangup :: Twiml' p }
instance NotGatherNoun p => Twiml p (Hangup p) where toTwiml' = fromHangup

hangup :: NotGatherNoun p => End p -> Hangup p
hangup = const . Hangup $ Fix HangupF
