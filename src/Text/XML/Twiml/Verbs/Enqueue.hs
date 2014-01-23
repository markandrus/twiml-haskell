{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeOperators #-}

module Text.XML.Twiml.Verbs.Enqueue
  ( -- * @\<Enqueue\>@
    -- $enqueue
    Enqueue
    -- ** Constructor
  , enqueue
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

{- $enqueue This example

@
module Example where

import Control.Lens
import Text.XML.Twiml

example
  = respond
  . undefined
  $ end
@

produces the following TwiML response:

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response\>
  \<Enqueue waitUrl=\"wait-music.xml\"\>support\<\/Enqueue\>
\<\/Response\>
@
-}

newtype Enqueue p = Enqueue { fromEnqueue :: Twiml' p }
instance (p :/~ Gather') => Twiml p (Enqueue  p) where toTwiml' = fromEnqueue

enqueue :: (Twiml p t, p :/~ Gather') => String -> t -> Enqueue p
enqueue name = Enqueue . Fix . EnqueueF defaultEnqueueAttributes name . toTwiml'
