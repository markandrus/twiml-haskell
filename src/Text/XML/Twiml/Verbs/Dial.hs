{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeOperators #-}

module Text.XML.Twiml.Verbs.Dial
  ( -- * @\<Dial\>@
    -- $dial
    Dial
    -- ** Constructors
  , dial
  , dial'
    -- ** Attributes
  , DialAttributes(..)
  , defaultDialAttributes
    -- *** Lenses
  , dialAttributes
  , hangupOnStar
  , timeLimit
  , callerId
  , recordDial
  , action
  , method
  , timeout
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

{- $dial This example

@
module Example where

import Control.Lens
import Text.XML.Twiml

example
  = respond
  . (dial (Right \"415-123-4567\") \<&\> timeout .~ 10
                                     record  .~ True)
  $ end
@

produces the following TwiML response:

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response\>
  \<Dial timeout=\"10\" record=\"true\"\>415-123-4567\<\/Dial\>
\<\/Response\>
@
-}

newtype Dial p = Dial { fromDial :: Twiml' p }
instance (p :/~ Gather') => Twiml p (Dial p) where toTwiml' = fromDial

dial :: (Twiml p t, p :/~ Gather') => Either DialNoun String -> t -> Dial p
dial = dial' defaultDialAttributes

dial' :: (Twiml p t, p :/~ Gather')
      => DialAttributes -> Either DialNoun String -> t -> Dial p
dial' attrs n = Dial . Fix . DialF attrs n . toTwiml'

dialAttributes :: Lens' (Dial p) DialAttributes
dialAttributes = lens
  (\(Dial (Fix (DialF attributes _ _))) -> attributes)
  (\(Dial (Fix (DialF _          n a)))    attributes ->
     Dial (Fix (DialF attributes n a)))

setDialAction :: DialAttributes -> URL -> DialAttributes
setDialAction attrs action = attrs { dialAction = Just action }

setDialMethod :: DialAttributes -> Method -> DialAttributes
setDialMethod attrs method = attrs { dialMethod = Just method }

setDialTimeout :: DialAttributes -> Natural -> DialAttributes
setDialTimeout attrs timeout = attrs { dialTimeout = Just timeout }

setDialHangupOnStar :: DialAttributes -> Bool -> DialAttributes
setDialHangupOnStar attrs hangupOnStar
  = attrs { dialHangupOnStar = Just hangupOnStar }

setDialTimeLimit :: DialAttributes -> Natural -> DialAttributes
setDialTimeLimit attrs timeLimit = attrs { dialTimeLimit = Just timeLimit }

setDialCallerId :: DialAttributes -> String -> DialAttributes
setDialCallerId attrs callerId = attrs { dialCallerId = Just callerId }

setDialRecord :: DialAttributes -> Bool -> DialAttributes
setDialRecord attrs record = attrs { dialRecord = Just record }

hangupOnStar :: Lens (Dial p) (Dial p) (Maybe Bool) Bool
hangupOnStar = lens (^. dialAttributes . to' dialHangupOnStar)
  (\t v -> over dialAttributes (flip setDialHangupOnStar v) t)

timeLimit :: Lens (Dial p) (Dial p) (Maybe Natural) Natural
timeLimit = lens (^. dialAttributes . to' dialTimeLimit)
  (\t v -> over dialAttributes (flip setDialTimeLimit v) t)

callerId :: Lens (Dial p) (Dial p) (Maybe String) String
callerId = lens (^. dialAttributes . to' dialCallerId)
  (\t v -> over dialAttributes (flip setDialCallerId v) t)

recordDial :: Lens (Dial p) (Dial p) (Maybe Bool) Bool
recordDial = lens (^. dialAttributes . to' dialRecord)
  (\t v -> over dialAttributes (flip setDialRecord v) t)

instance HasAction (Dial p) where
  action = lens getAction setAction where
    getAction = (^. dialAttributes . to' dialAction)
    setAction t v = over dialAttributes (flip setDialAction v) t

instance HasMethod (Dial p) where
  method = lens getMethod setMethod where
    getMethod = (^. dialAttributes . to' dialMethod)
    setMethod t v = over dialAttributes (flip setDialMethod v) t

instance HasTimeout (Dial p) where
  timeout = lens getTimeout setTimeout where
    getTimeout = (^. dialAttributes . to' dialTimeout)
    setTimeout t v = over dialAttributes (flip setDialTimeout v) t


