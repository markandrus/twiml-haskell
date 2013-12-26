{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.End where

import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..), Fix(..))

newtype End p = End { fromEnd :: Twiml' p }
instance Twiml p (End p) where toTwiml' = fromEnd

end :: End p
end = End . Fix $ EndF
