{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Enqueue
  ( -- * @\<Enqueue\>@
    Enqueue
    -- ** Constructor
  , enqueue
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

newtype Enqueue p = Enqueue { fromEnqueue :: Twiml' p }
instance NotGatherNoun p => Twiml p (Enqueue  p) where toTwiml' = fromEnqueue

enqueue :: (Twiml p t, NotGatherNoun p) => String -> t -> Enqueue p
enqueue name = Enqueue . Fix . EnqueueF defaultEnqueueAttributes name . toTwiml'
