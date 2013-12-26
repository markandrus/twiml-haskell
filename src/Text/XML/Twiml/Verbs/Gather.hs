{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Gather where

import Text.XML.Twiml.Internal (Fix(..), Lang(..), LangAlice(..), GatherAttributes(..), setGatherNumDigits, defaultGatherAttributes, Twiml(..), Twiml', TwimlF(..), URL(..), NotGatherNoun, GatherNoun, Natural)
import Text.XML.Twiml.Internal.Lens ((^.), Lens, Lens', lens, over, to')

newtype Gather p = Gather { fromGather :: Twiml' p }
instance NotGatherNoun p => Twiml p (Gather   p) where toTwiml' = fromGather

gather :: (Twiml GatherNoun n, Twiml p t, NotGatherNoun p) => n -> t -> Gather p
gather = gather' defaultGatherAttributes

gather' :: (Twiml GatherNoun n, Twiml p t, NotGatherNoun p)
        => GatherAttributes -> n -> t -> Gather p
gather' attrs n
  = Gather . Fix . GatherF attrs (toTwiml' n) . toTwiml'

gatherAttributes :: Lens' (Gather p) GatherAttributes
gatherAttributes = lens
  (\(Gather (Fix (GatherF attributes _ _))) -> attributes)
  (\(Gather (Fix (GatherF _          n a)))    attributes ->
     Gather (Fix (GatherF attributes n a)))

numDigits :: Lens (Gather p) (Gather p) (Maybe Natural) Natural
numDigits = lens (^. gatherAttributes . to' gatherNumDigits)
  (\t v -> over gatherAttributes (flip setGatherNumDigits v) t)
