{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Text.XML.Twiml.Verbs.Reject where

import Text.XML.Twiml.Verbs.End (End)
import Text.XML.Twiml.Internal (Fix(..), Lang(..), LangAlice(..), Twiml(..), Twiml', TwimlF(..), URL(..), NotGatherNoun, Natural, RejectAttributes(..), defaultRejectAttributes, setRejectReason, Reason(..))
import Text.XML.Twiml.Internal.Lens ((^.), Lens, Lens', lens, over, to')

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

reason :: Lens (Reject p) (Reject p) (Maybe Reason) Reason
reason = lens (^. rejectAttributes . to' rejectReason)
  (\t v -> over rejectAttributes (flip setRejectReason v) t)

