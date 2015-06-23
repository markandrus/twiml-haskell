{-#LANGUAGE DataKinds #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE PolyKinds #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE TypeOperators #-}

module Text.XML.Twiml.Syntax where

import Data.Default
import Prelude (const)
import Text.XML.Twiml.Types

data Syntax (m :: [k] -> * -> *) (i :: [k]) (j :: [k]) (a :: *) (b :: *) = Syntax {
    (>>=)  :: m i a -> (a -> m j b) -> m (i <> j) b
  , (>>)   :: m i a -> m j b -> m (i <> j) b
  , return :: a -> m Identity a
  }

instance IxMonad m => Default (Syntax m i j a b) where
  def = Syntax ibind (\a b -> a `ibind` const b) ipure
