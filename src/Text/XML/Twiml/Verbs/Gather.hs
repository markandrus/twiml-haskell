{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE RankNTypes #-}

module Text.XML.Twiml.Verbs.Gather
  ( -- * @\<Gather\>@
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

newtype Gather p = Gather { fromGather :: Twiml' p }
instance NotGatherNoun p => Twiml p (Gather   p) where toTwiml' = fromGather

gather :: (Twiml GatherNoun n, Twiml p t, NotGatherNoun p) => n -> t -> Gather p
gather = gather' defaultGatherAttributes

gather' :: (Twiml GatherNoun n, Twiml p t, NotGatherNoun p)
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

instance HasTimeout (Gather p) where
  timeout = lens getTimeout setTimeout where
    getTimeout = (^. gatherAttributes . to' gatherTimeout)
    setTimeout t v = over gatherAttributes (flip setGatherTimeout v) t

instance HasFinishOnKey (Gather p) where
  finishOnKey = lens getFinishOnKey setFinishOnKey where
    getFinishOnKey = (^. gatherAttributes . to' gatherFinishOnKey)
    setFinishOnKey t v = over gatherAttributes (flip setGatherFinishOnKey v) t
