{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE RankNTypes #-}

module Text.XML.Twiml
  ( Response
  , respond
  , module Text.XML.Twiml.Verbs
  , module Text.XML.Twiml.Types
  , module Text.XML.Twiml.Internal
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Verbs
import Text.XML.Twiml.Types hiding
  ( Lens
  , Lens'
  , lens
  , (^.)
  , over
  , to'
  , Fix
  , Foldable
  , Base
  , Yes
  , No
  )

{- Twiml Response -}

-- | The root element of Twilio's XML Markup is the @\<Response\>@ element. See
-- <https://www.twilio.com/docs/api/twiml/your_response#response-element>.
newtype Response = Response { fromResponse :: Twiml' Response }

instance Twiml Response Response where
  toTwiml' = fromResponse

instance Show Response where
  show = show . toTwiml'

respond :: Twiml Response t => t -> Response
respond = Response . toTwiml'
