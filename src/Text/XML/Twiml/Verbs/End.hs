{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Verbs.End
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- End is not actually a TwiML verb; it is used for terminating TwiML.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Verbs.End
  ( end
  , End
  , EndF(..)
  ) where

import Text.XML.Twiml.Internal
import Text.XML.Twiml.Internal.Twiml

end :: IsTwimlLike f End => TwimlLike f End a
end = iliftF $ inj EndF
