{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Leave where

import Text.XML.Twiml.Verbs.End (End)
import Text.XML.Twiml.Internal (Fix(..), Lang(..), LangAlice(..), Twiml(..), Twiml', TwimlF(..), URL(..), NotGatherNoun, Natural)
import Text.XML.Twiml.Internal.Lens ((^.), Lens, Lens', lens, over, to')

newtype Leave p = Leave { fromLeave :: Twiml' p }
instance NotGatherNoun p => Twiml p (Leave p) where toTwiml' = fromLeave

leave :: NotGatherNoun p => End p -> Leave p
leave = const . Leave $ Fix LeaveF
