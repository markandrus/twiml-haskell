{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE TypeOperators #-}

module Text.XML.Twiml.Verbs.Gather
  ( -- * @\<Gather\>@
    -- $gather
    Gather
    -- ** Constructors
  , gather
  , gather'
    -- ** Attributes
  , GatherAttributes(..)
  , defaultGatherAttributes
    -- *** Lenses
  , gatherAttributes
  , numDigits
  , action
  , method
  , timeout
  , finishOnKey
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))
import Text.XML.Twiml.Verbs.End

import Unsafe.Coerce (unsafeCoerce)

{- $gather This example

@
module Example where

import Control.Lens
import Text.XML.Twiml

example
  = respond
  . (gather \<&\> timeout     .~ 10
            \<&\> finishOnKey .~ KStar
      $ say \"Please enter your pin number and then press star.\"
      $ end)
  $ end
@

produces the following TwiML response:

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response\>
  \<Gather timeout=\"10\" finishOnKey=\"*\"\>
    \<Say\>Please enter your pin number and then press star.\<\/Say\>
  \<\/Gather\>
\<\/Response\>
@
-}

newtype Gather p = Gather { fromGather :: Twiml' p }
instance (p :/~ Gather') => Twiml p (Gather p) where toTwiml' = fromGather

gather :: (Twiml Gather' n, Twiml p t, p :/~ Gather') => n -> t -> Gather p
gather = gather' defaultGatherAttributes

gather' :: (Twiml Gather' n, Twiml p t, p :/~ Gather')
        => GatherAttributes -> n -> t -> Gather p
gather' attrs n
  = Gather . Fix . GatherF attrs (toTwiml' n) . toTwiml'

gatherAttributes :: Lens' (Gather p) GatherAttributes
gatherAttributes = lens
  (\(Gather (Fix (GatherF attributes _ _))) -> attributes)
  (\(Gather (Fix (GatherF _          n a)))    attributes ->
     Gather (Fix (GatherF attributes n a)))

setGatherAction :: GatherAttributes -> URL -> GatherAttributes
setGatherAction attrs action = attrs { gatherAction = Just action }

setGatherMethod :: GatherAttributes -> Method -> GatherAttributes
setGatherMethod attrs method = attrs { gatherMethod = Just method }

setGatherTimeout :: GatherAttributes -> Natural -> GatherAttributes
setGatherTimeout attrs timeout = attrs { gatherTimeout = Just timeout }

setGatherFinishOnKey :: GatherAttributes -> Key -> GatherAttributes
setGatherFinishOnKey attrs finishOnKey
  = attrs { gatherFinishOnKey = Just finishOnKey }

setGatherNumDigits :: GatherAttributes -> Natural -> GatherAttributes
setGatherNumDigits attrs numDigits = attrs { gatherNumDigits = Just numDigits }

numDigits :: Lens (Gather p) (Gather p) (Maybe Natural) Natural
numDigits = lens (^. gatherAttributes . to' gatherNumDigits)
  (\t v -> over gatherAttributes (flip setGatherNumDigits v) t)

instance forall p t. HasAction (t -> Gather p) where
  action = lens getAction setAction where
    getAction f = (^. gatherAttributes . to' gatherAction) (f $ unsafeCoerce end)
    setAction f v = fmap (over gatherAttributes (flip setGatherAction v)) f

instance forall p t. Twiml p t => HasMethod (t -> Gather p) where
  method = lens getMethod setMethod where
    getMethod f = (^. gatherAttributes . to' gatherMethod) (f $ unsafeCoerce end)
    setMethod f v = fmap (over gatherAttributes (flip setGatherMethod v)) f

instance forall p t. Twiml p t => HasTimeout (t -> Gather p) where
  timeout = lens getTimeout setTimeout where
    getTimeout f = (^. gatherAttributes . to' gatherTimeout) (f $ unsafeCoerce end)
    setTimeout f v = fmap (over gatherAttributes (flip setGatherTimeout v)) f

instance forall p t. Twiml p t => HasFinishOnKey (t -> Gather p) where
  finishOnKey = lens getFinishOnKey setFinishOnKey where
    getFinishOnKey f = (^. gatherAttributes . to' gatherFinishOnKey) (f $ unsafeCoerce end)
    setFinishOnKey f v = fmap (over gatherAttributes (flip setGatherFinishOnKey v)) f
