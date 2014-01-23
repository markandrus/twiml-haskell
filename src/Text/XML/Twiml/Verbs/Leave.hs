{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Leave
  ( -- * @\<Leave\>@
    Leave
    -- ** Constructor
  , leave
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Verbs.End (End)
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

newtype Leave p = Leave { fromLeave :: Twiml' p }
instance NotGatherNoun p => Twiml p (Leave p) where toTwiml' = fromLeave

leave :: NotGatherNoun p => End p -> Leave p
leave = const . Leave $ Fix LeaveF
