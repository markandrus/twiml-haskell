{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Reject
  ( -- * @\<Reject\>@
    Reject
  , reject
  , reject'
    -- * Attribute Lenses
  , reason
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Verbs.End (End)
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

newtype Reject   p = Reject   { fromReject   :: Twiml' p }
instance NotGatherNoun p => Twiml p (Reject   p) where toTwiml' = fromReject

reject :: NotGatherNoun p => End p -> Reject p
reject = reject' defaultRejectAttributes

reject' :: NotGatherNoun p => RejectAttributes -> End p -> Reject p
reject' attrs = const . Reject . Fix $ RejectF attrs

rejectAttributes :: Lens' (Reject p) RejectAttributes
rejectAttributes = lens
  (\(Reject (Fix (RejectF attributes))) -> attributes)
  (\(Reject (Fix (RejectF _         )))    attributes ->
     Reject (Fix (RejectF attributes)))

setRejectReason :: RejectAttributes -> Reason -> RejectAttributes
setRejectReason attrs reason = attrs { rejectReason = Just reason }

reason :: Lens (Reject p) (Reject p) (Maybe Reason) Reason
reason = lens (^. rejectAttributes . to' rejectReason)
  (\t v -> over rejectAttributes (flip setRejectReason v) t)

