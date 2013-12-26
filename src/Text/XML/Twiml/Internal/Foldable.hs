{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TypeFamilies #-}

module Text.XML.Twiml.Internal.Foldable where

{- Fix and Foldable -}

-- $fixAndFoldable The following definitions were extracted from the recursion-schemes package.

newtype Fix f = Fix { unFix :: f (Fix f) }

type family Base t :: * -> *

class Functor (Base t) => Foldable t where
  project :: t -> Base t t
  cata :: (Base t a -> a) -> t -> a
  cata f = c where c = f . fmap c . project
