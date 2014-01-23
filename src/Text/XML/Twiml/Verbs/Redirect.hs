{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeOperators #-}

module Text.XML.Twiml.Verbs.Redirect
  ( -- * @\<Redirect\>@
    -- $redirect
    Redirect
    -- ** Constructors
  , redirect
  , redirect'
    -- ** Attributes
  , RedirectAttributes(..)
  , defaultRedirectAttributes
    -- *** Lenses
  , redirectAttributes
  , method
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Verbs.End (End)
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

{- $redirect This example

@
module Example where

import Control.Lens
import Data.Maybe (fromJust)
import Text.XML.Twiml

example
  = respond
  . (redirect (fromJust $ parseURL \"http:\/\/pigeons.com\/twiml.xml\") \<&\> method .~ POST)
  $ end
@

produces the following TwiML response:

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response\>
  \<Redirect method=\"POST\"\>http:\/\/pigeons.com\/twiml.xml\<\/Redirect\>
\<\/Response\>
@
-}

newtype Redirect p = Redirect { fromRedirect :: Twiml' p }
instance (p :/~ Gather') => Twiml p (Redirect p) where toTwiml' = fromRedirect

redirect :: (p :/~ Gather') => URL -> End p -> Redirect p
redirect = redirect' defaultRedirectAttributes

redirect' :: (p :/~ Gather') => RedirectAttributes -> URL -> End p -> Redirect p
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


