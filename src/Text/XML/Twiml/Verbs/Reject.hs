{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeOperators #-}

module Text.XML.Twiml.Verbs.Reject
  ( -- * @\<Reject\>@
    -- $reject
    Reject
    -- ** Constructors
  , reject
  , reject'
    -- ** Attributes
  , RejectAttributes(..)
  , defaultRejectAttributes
    -- *** Lenses
  , rejectAttributes
  , reason
  ) where

import Text.XML.Twiml.Types
import Text.XML.Twiml.Verbs.End (End)
import Text.XML.Twiml.Internal (Twiml(..), Twiml', TwimlF(..))

{- $reject This example

@
module Example where

import Control.Lens
import Text.XML.Twiml

example
  = respond
  . (reject \<&\> reason .~ Busy)
  $ end
@

produces the following TwiML response:

@
\<?xml version=\"1.0\" encoding=\"UTF-8\"?\>
\<Response\>
  \<Reject reason=\"busy\" \/\>
\<\/Response\>
@
-}

newtype Reject p = Reject { fromReject :: Twiml' p }
instance (p :/~ Gather') => Twiml p (Reject p) where toTwiml' = fromReject

reject :: (p :/~ Gather') => End p -> Reject p
reject = reject' defaultRejectAttributes

reject' :: (p :/~ Gather') => RejectAttributes -> End p -> Reject p
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
  (\t v -> over rejectAttributes (`setRejectReason` v) t)

