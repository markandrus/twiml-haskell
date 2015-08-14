{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Internal
-- Copyright   :  (C) 2014-15 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- This module exports the machinery necessary to define TwiML in an extensible
-- way.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Internal
  ( -- * Data types à la carte
    -- $datatypes
    (:+:)(..)
  , (:<:)(..)
    -- * Elem (∉)
  , Elem
  , type (∉)
    -- * Indexed
    -- $indexed
  , Functor1(..)
  , NFData1(..)
  , Show1(..)
    -- ** Applicative
  , IxApplicative(..)
    -- ** Monad
  , IxMonad(..)
    -- ** Free
  , IxFree(..)
  , iliftF
  , type (++)
    -- * XML
    -- $xml
  , SomeNode(..)
  , ToSomeNode(..)
  , ToXML(..)
  , ToElement(..)
  , ToAttrs(..)
  , ToAttrValue(..)
  , makeAttr
  , makeAttr'
  , makeAttrs
  , makeElement
  ) where

import Control.DeepSeq (NFData(..))
import Data.Data
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Text.XML.Light

{- Data types à la carte -}

-- $datatypes The @(':+:')@ data type and @(':<:')@ type class come from
-- Swierstra's
-- <http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf Data types à la carte>.

data (f :+: g) a = InL (f a) | InR (g a)
  deriving (Eq, Functor, Generic, NFData, Ord, Read, Show)

infixr 7 :+:

deriving instance (Data a, Data (f a), Data (g a), Typeable f, Typeable g) => Data ((f :+: g) a)

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance Functor f => f :<: f where
  inj = id
  prj = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = InL
  prj (InL f) = Just f
  prj _ = Nothing

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = InR . inj
  prj (InR g) = prj g
  prj _ = Nothing

{- Elem -}

-- $elem 'TwimlF uses @∉@ in order to enforce nesting rules.

-- | 'Elem' is like a promoted @elem@: it allows us to check whether a type
-- constructor @t@ is present in a list of type constructors @ts@.
type family Elem (t :: k) (ts :: [k]) :: Bool where
  Elem t '[] = 'False
  Elem t (t ': ts) = 'True
  Elem t (u ': ts) = Elem t ts

{- @(∉)@ -}

-- | @t ∉ ts@ is shorthand for asserting that a type constructor @t@ is not
-- present in a list of types constructors @ts@.
type t ∉ ts = Elem t ts ~ 'False

{- Indexed -}

-- $indexed Everything in this section comes from
-- <http://stackoverflow.com/a/27682570 Cirdec's excellent answer> on a
-- StackOverflow question about indexed free monads. Some names have been
-- changed to follow the patterns established by
-- <https://hackage.haskell.org/package/indexed indexed> and
-- <https://hackage.haskell.org/package/indexed-free indexed-free>.

class Functor1 f where
  fmap1 :: (a -> b) -> f i a -> f i b

class Show1 f where
  show1 :: Show a => f i a -> String

{- Applicative -}

-- | An applicative functor @f@ indexed by a monoid @(M,'<>','Identity')@
--
-- If you import @Prelude@ hiding @(\<*>)@ and @pure@, you can redefine
-- @(\<*>)@ and @pure@ to use their indexed equivalents. For example,
--
-- @
-- import Prelude hiding ((\<*>), pure)
--
-- pure :: 'IxApplicative' f -> a -> f 'Identity' a
-- pure = 'ipure'
--
-- (\<*>) :: 'IxApplicative' f => f i (a -> b) -> f j a -> f (i '<>' j) b
-- (\<*>) = 'iap'
-- @
class Functor1 f => IxApplicative (f :: k -> * -> *) where
  type Identity :: k

  type (i :: k) <> (j :: k) :: k

  -- | The indexed equivalent of @pure@
  ipure :: a -> f Identity a

  -- | The indexed equivalent of @(\<*>)@
  iap :: f i (a -> b) -> f j a -> f (i <> j) b

{- Monad -}

-- | A monad @m@ indexed by a monoid @(M,'<>','Identity')@
--
-- You can use do-notation with 'IxMonad' by enabling the RebindableSyntax
-- extension and redefining @(>>=)@, @(>>)@, and @return@. For example,
--
-- @
-- {-\#LANGUAGE RebindableSyntax #-}
--
-- import Prelude hiding ((>>=), (>>), return)
--
-- (>>=) :: 'IxMonad' m => m i a -> (a -> m j b) -> m (i '<>' j) b
-- (>>=) = 'ibind'
--
-- (>>) :: 'IxMonad' m => m i a -> m j b -> m (i '<>' j) b
-- a >> b = a >>= const b
--
-- return :: 'IxApplicative' m => a -> m 'Identity' a
-- return = 'ipure'
-- @
--
-- This is the technique employed by the
-- <Text-XML-Twiml-Syntax.html Text.XML.Twiml.Syntax> module.
class IxApplicative m => IxMonad (m :: k -> * -> *) where
  -- | The indexed equivalent of @(>>=)@
  ibind :: m i a -> (a -> m j b) -> m (i <> j) b

{- Free -}

-- | A free monad indexed by a monoid @(M,'++',[])@
data IxFree f (i :: [k]) a where
  IxPure :: a -> IxFree f '[] a
  IxFree :: WitnessList i => f i (IxFree f j a) -> IxFree f (i ++ j) a

instance (Show1 f, Show a) => Show (IxFree f i a) where
  show (IxPure a) = "IxPure (" ++ show a ++ ")"
  show (IxFree fa) = "IxFree (" ++ show1 fa ++ ")"

instance Show1 f => Show1 (IxFree f) where
  show1 = show

instance Functor1 f => Functor (IxFree f i) where
  fmap = fmap1

instance Functor1 f => Functor1 (IxFree f) where
  fmap1 = fmap

instance Functor1 f => IxApplicative (IxFree f) where
  type Identity = '[]

  type i <> j = i ++ j

  ipure = IxPure

  iap = iap'

iap'
  :: forall f i j a b. Functor1 f
  => IxFree f i (a -> b) -> IxFree f j a -> IxFree f (i ++ j) b
iap' (IxPure f) (IxPure a) = IxPure $ f a
iap' (IxPure f) (IxFree mb) = IxFree $ fmap1 (fmap f) mb
iap' (IxFree (mf :: f i1 (IxFree f j1 (a -> b)))) a =
  case associativity (witness :: SList i1) (Proxy :: Proxy j1) (Proxy :: Proxy j)
  of Refl -> IxFree $ fmap1 (`iap'` a) mf

instance (Functor1 m, IxApplicative (IxFree m)) => IxMonad (IxFree m) where
  ibind = ibind'

ibind'
  :: forall f i j a b. Functor1 f
  => IxFree f i a -> (a -> IxFree f j b) -> IxFree f (i ++ j) b
ibind' (IxPure a) f = f a
ibind' (IxFree (x :: f i1 (IxFree f j1 a))) f =
    case associativity (witness :: SList i1) (Proxy :: Proxy j1) (Proxy :: Proxy j)
    of Refl -> IxFree $ fmap1 (`ibind'` f) x

-- | Lift an indexed functor into 'IxFree'
iliftF :: forall f i a . (WitnessList i, Functor1 f) => f i a -> IxFree f i a
iliftF = case rightIdentity (witness :: SList i) of Refl -> IxFree . fmap1 IxPure

class NFData1 f where
  rnf1 :: NFData a => f i a -> ()

instance NFData1 f => NFData1 (IxFree f) where
  rnf1 = rnf

instance (NFData1 f, NFData a) => NFData (IxFree f i a) where
  rnf (IxPure a) = rnf a
  rnf (IxFree fa) = rnf1 fa

{- Promoted Lists -}

-- $promotedLists 'IxFree' relies on a promoted list of type constructors, so
-- we'll need
--
-- * promoted list concatenation, @('++')@,
-- * a proof that @('++')@ associates (used by 'iap' and 'ibind'), and
-- * a right identity proof for @[]@ (used by 'iliftF).

{- (++) -}

-- | Promoted list concatenation
type family (++) (a :: [k]) (b :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': as ++ bs

-- | 'SList' is the singleton type for promoted lists.
data SList (i :: [k]) where
  Nil :: SList '[]
  Succ :: SList t -> SList (h ': t)

class WitnessList (xs :: [k]) where
  witness :: SList xs

instance WitnessList '[] where
  witness = Nil

instance WitnessList xs => WitnessList (x ': xs) where
  witness = Succ witness

-- | A proof that @('++')@ associates, i.e.
--
-- @
-- xs ++ (ys ++ zs) ≡ (xs ++ ys) ++ zs
-- @
associativity :: SList xs -> Proxy ys -> Proxy zs
         -> (xs ++ (ys ++ zs)) :~: ((xs ++ ys) ++ zs)
associativity Nil _ _ = Refl
associativity (Succ xs) ys zs =
  case associativity xs ys zs of Refl -> Refl

-- | A proof that
--
-- @
-- xs ≡ xs ++ []
-- @
rightIdentity :: SList xs -> xs :~: (xs ++ '[])
rightIdentity Nil = Refl
rightIdentity (Succ xs) = case rightIdentity xs of Refl -> Refl

{- XML -}

-- $xml The classes here simplify working with the
-- <https://hackage.haskell.org/package/xml xml> package.

data SomeNode = forall n. Node n => SomeNode n

class ToSomeNode a where
  toSomeNode :: a -> SomeNode

instance ToSomeNode a => ToSomeNode (Maybe a) where
  toSomeNode (Just a) = toSomeNode a
  toSomeNode _ = SomeNode ()

instance Node SomeNode where
  node qName (SomeNode n) = node qName n

instance ToSomeNode String where
  toSomeNode str = SomeNode . Text $ CData CDataText str Nothing

instance ToSomeNode () where
  toSomeNode = SomeNode

instance ToSomeNode n => Node n where
  node qName n = node qName (toSomeNode n)

class ToXML a where
  toXML :: a -> [Element]

instance (ToXML (f a), ToXML (g a)) => ToXML ((f :+: g) a) where
  toXML (InL f) = toXML f
  toXML (InR g) = toXML g

class ToElement a where
  toElement :: a -> Element

class ToAttrs a where
  toAttrs :: a -> [Attr]

-- | 'toAttrValue' transforms a data type into a @String@ that can be set as the
-- value of an attribute.
class ToAttrValue a where
  toAttrValue :: a -> String

instance ToAttrValue Bool where
  toAttrValue True  = "true"
  toAttrValue False = "false"

instance ToAttrValue String where
  toAttrValue = id

makeAttr :: ToAttrValue b => String -> (a -> Maybe b) -> a -> Maybe Attr
makeAttr str f a = Attr (unqual str) . toAttrValue <$> f a

makeAttr' :: String -> (a -> Maybe b) -> (b -> String) -> a -> Maybe Attr
makeAttr' str f g a = Attr (unqual str) . g <$> f a

makeAttrs :: a -> [a -> Maybe Attr] -> [Attr]
makeAttrs a = mapMaybe ($ a)

makeElement :: Node t => String -> t -> [Attr] -> Element
makeElement str c attrs = add_attrs attrs $ unode str c
