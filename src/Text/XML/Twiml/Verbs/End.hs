{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.End
  ( -- * End
    -- $end
    End
  , end
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

-- $end 'End' is not actually a TwiML verb but is used to terminate 'Twiml'.
-- Note that
--
-- @
-- respond $ end
-- @
--
-- produces an empty TwiML response:
--
-- @
-- <Response>
-- </Response>
-- @

newtype End p = End { fromEnd :: Twiml' p }
instance Twiml p (End p) where toTwiml' = fromEnd

end :: End p
end = End . Fix $ EndF
