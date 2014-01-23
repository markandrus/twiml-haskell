{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.End
  ( -- * End
    -- $end
    End
    -- ** Constructor
  , end
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

{- $end 'End' is not actually a TwiML verb, but this library uses it to
terminate 'Twiml'. This example

@
module Example where

import Text.XML.Twiml

example
  = respond
  $ end
@

produces the following empty TwiML response:

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response \/\>
@
-}

newtype End p = End { fromEnd :: Twiml' p }
instance Twiml p (End p) where toTwiml' = fromEnd

end :: End p
end = End . Fix $ EndF
