{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Enqueue where

import Text.XML.Twiml.Internal (Fix(..), Lang(..), LangAlice(..), EnqueueAttributes(..), defaultEnqueueAttributes, Twiml(..), Twiml', TwimlF(..), URL(..), NotGatherNoun, Natural)
import Text.XML.Twiml.Internal.Lens ((^.), Lens, Lens', lens, over, to')

newtype Enqueue p = Enqueue { fromEnqueue :: Twiml' p }
instance NotGatherNoun p => Twiml p (Enqueue  p) where toTwiml' = fromEnqueue

enqueue :: (Twiml p t, NotGatherNoun p) => String -> t -> Enqueue p
enqueue name = Enqueue . Fix . EnqueueF defaultEnqueueAttributes name . toTwiml'
