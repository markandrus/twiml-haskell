{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Redirect where

import Text.XML.Twiml.Verbs.End (End)
import Text.XML.Twiml.Internal (Fix(..), Lang(..), LangAlice(..), Twiml(..), Twiml', TwimlF(..), URL(..), NotGatherNoun, Natural, RedirectAttributes, defaultRedirectAttributes)
import Text.XML.Twiml.Internal.Lens ((^.), Lens, Lens', lens, over, to')

newtype Redirect p = Redirect { fromRedirect :: Twiml' p }
instance NotGatherNoun p => Twiml p (Redirect p) where toTwiml' = fromRedirect

redirect :: NotGatherNoun p => URL -> End p -> Redirect p
redirect = redirect' defaultRedirectAttributes

redirect' :: NotGatherNoun p => RedirectAttributes -> URL -> End p -> Redirect p
redirect' attrs url = const . Redirect . Fix $ RedirectF attrs url

redirectAttributes :: Lens' (Redirect p) RedirectAttributes
redirectAttributes = lens
  (\(Redirect (Fix (RedirectF attributes _))) -> attributes)
  (\(Redirect (Fix (RedirectF _          n)))    attributes ->
     Redirect (Fix (RedirectF attributes n)))
