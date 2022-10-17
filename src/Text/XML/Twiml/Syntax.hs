{-# LANGUAGE CPP #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE PolyKinds #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE TypeOperators #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Syntax
-- Copyright   :  (C) 2018 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- This module, in combination with the @RebindableSyntax@ and @RecordWilCards@
-- extensions, allows you to write TwiML using do-notation. For example,
--
-- @
-- {-\# LANGUAGE RebindableSyntax \#-}
-- {-\# LANGUAGE RecordWildCards \#-}
--
-- import Prelude
-- import Data.Default
-- import Text.XML.Twiml
-- import qualified Text.XML.Twiml.Syntax as Twiml
--
-- example :: 'VoiceTwiml'
-- example =
--   'response' $ do
--     'say' "Hello World" def
--     'end'
--   where Twiml.'Syntax'{..} = def
-- @
--
-- This pattern is due to a
-- <https://mail.haskell.org/pipermail/haskell-cafe/2015-June/120222.html suggestion from Adam Bergmark on Haskell-Cafe>.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Syntax where

import Data.Default
#if MIN_VERSION_base(4,9,0)
import Data.Kind (Type)
#endif
import Prelude (const)
import Text.XML.Twiml.Internal

#if MIN_VERSION_base(4,9,0)
data Syntax (m :: [k] -> Type -> Type) (i :: [k]) (j :: [k]) (a :: Type) (b :: Type) = Syntax {
#else
data Syntax (m :: [k] -> * -> *) (i :: [k]) (j :: [k]) (a :: *) (b :: *) = Syntax {
#endif
    (>>=)  :: m i a -> (a -> m j b) -> m (i <> j) b
  , (>>)   :: m i a -> m j b -> m (i <> j) b
  , return :: a -> m Identity a
  }

instance IxMonad m => Default (Syntax m i j a b) where
  def = Syntax ibind (\a b -> a `ibind` const b) ipure
