{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Redirect
  ( -- * @\<Redirect\>@
    Redirect
  , redirect
  , redirect'
    -- * Attribute Lenses
  , method
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Verbs.End (End)
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

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

setRedirectMethod :: RedirectAttributes -> Method -> RedirectAttributes
setRedirectMethod attrs method = attrs { redirectMethod = Just method }

instance HasMethod (Redirect p) where
  method = lens getMethod setMethod where
    getMethod = (^. redirectAttributes . to' redirectMethod)
    setMethod t v = over redirectAttributes (flip setRedirectMethod v) t


