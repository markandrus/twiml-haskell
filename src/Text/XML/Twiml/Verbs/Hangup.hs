{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeOperators #-}

module Text.XML.Twiml.Verbs.Hangup
  ( -- * @\<Hangup\>@
    -- $hangup
    Hangup
    -- ** Constructor
  , hangup
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Verbs.End (End)
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

{- $hangup This example

@
module Example where

import Text.XML.Twiml

example
  = respond
  . hangup
  $ end
@

produces the following TwiML response:

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response\>
  \<Hangup \/\>
\<\/Response\>
@
-}

newtype Hangup p = Hangup { fromHangup :: Twiml' p }
instance (p :/~ Gather') => Twiml p (Hangup p) where toTwiml' = fromHangup

hangup :: (p :/~ Gather') => End p -> Hangup p
hangup = const . Hangup $ Fix HangupF
