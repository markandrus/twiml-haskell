{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE RankNTypes #-}

module Text.XML.Twiml
  ( Response
  , respond
  , Text.XML.Twiml.Verbs
  ) where

import Text.XML.Twiml.Verbs
import Text.XML.Twiml.Internal hiding (Lens, lens)
import Text.XML.Twiml.Internal.Lens

import Unsafe.Coerce (unsafeCoerce)

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
