{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeOperators #-}

module Text.XML.Twiml.Verbs.Leave
  ( -- * @\<Leave\>@
    -- $leave
    Leave
    -- ** Constructor
  , leave
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Verbs.End (End)
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

{- $leave This example

@
module Example where

import Text.XML.Twiml

example
  = respond
  . leave
  $ end
@

produces the following TwiML response:

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response\>
  \<Leave \/\>
\<\/Response\>
@
-}

newtype Leave p = Leave { fromLeave :: Twiml' p }
instance (p :/~ Gather') => Twiml p (Leave p) where toTwiml' = fromLeave

leave :: (p :/~ Gather') => End p -> Leave p
leave = const . Leave $ Fix LeaveF
