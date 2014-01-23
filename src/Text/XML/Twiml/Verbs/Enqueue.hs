{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeOperators #-}

module Text.XML.Twiml.Verbs.Enqueue
  ( -- * @\<Enqueue\>@
    -- $enqueue
    Enqueue
    -- ** Constructors
  , enqueue
  , enqueue'
    -- ** Attributes
  , EnqueueAttributes(..)
  , defaultEnqueueAttributes
    -- *** Lenses
  , enqueueAttributes
  , action
  , method
  , waitURL
  , waitURLMethod
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

{- $enqueue This example

@
module Example where

import Control.Lens
import Data.Maybe (fromJust)
import Text.XML.Twiml

example
  = respond
  . (enqueue \"support\" \<&\> waitURL .~ (fromJust $ parseURL \"wait-music.xml\"))
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

enqueue' :: (Twiml p t, p :/~ Gather') => EnqueueAttributes -> String -> t -> Enqueue p
enqueue' attrs name = Enqueue . Fix . EnqueueF attrs name . toTwiml'

enqueueAttributes :: Lens' (Enqueue p) EnqueueAttributes
enqueueAttributes = lens
  (\(Enqueue (Fix (EnqueueF attributes _ _))) -> attributes)
  (\(Enqueue (Fix (EnqueueF _          n a)))    attributes ->
     Enqueue (Fix (EnqueueF attributes n a)))

setEnqueueAction :: EnqueueAttributes -> URL -> EnqueueAttributes
setEnqueueAction attrs action = attrs { enqueueAction = Just action }

setEnqueueMethod :: EnqueueAttributes -> Method -> EnqueueAttributes
setEnqueueMethod attrs method = attrs { enqueueMethod = Just method }

setEnqueueWaitURL :: EnqueueAttributes -> URL -> EnqueueAttributes
setEnqueueWaitURL attrs url = attrs { enqueueWaitURL = Just url }

setEnqueueWaitURLMethod :: EnqueueAttributes -> Method -> EnqueueAttributes
setEnqueueWaitURLMethod attrs meth = attrs { enqueueWaitURLMethod = Just meth }

instance HasAction (Enqueue p) where
  action = lens getAction setAction where
    getAction = (^. enqueueAttributes . to' enqueueAction)
    setAction t v = over enqueueAttributes (flip setEnqueueAction v) t

instance HasMethod (Enqueue p) where
  method = lens getMethod setMethod where
    getMethod = (^. enqueueAttributes . to' enqueueMethod)
    setMethod t v = over enqueueAttributes (flip setEnqueueMethod v) t

waitURL :: Lens (Enqueue p) (Enqueue p) (Maybe URL) URL
waitURL = lens (^. enqueueAttributes . to' enqueueWaitURL)
  (\t v -> over enqueueAttributes (flip setEnqueueWaitURL v) t)

waitURLMethod :: Lens (Enqueue p) (Enqueue p) (Maybe Method) Method
waitURLMethod = lens (^. enqueueAttributes . to' enqueueWaitURLMethod)
  (\t v -> over enqueueAttributes (flip setEnqueueWaitURLMethod v) t)
