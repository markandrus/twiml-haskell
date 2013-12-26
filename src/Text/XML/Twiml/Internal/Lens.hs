{-#LANGUAGE RankNTypes #-}

module Text.XML.Twiml.Internal.Lens
  ( (^.)
  , Lens
  , Lens'
  , lens
  , to'
  , over
  ) where

import Unsafe.Coerce

{- Basic Lens Functionality -}

-- The following section extracts a number of definitions required to get
-- lenses, as defined in the lens package, working, without relying on the lens
-- package itself. If this turns out to be a bad idea, please email me.

-- The following definitions were extracted from the lens package.

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = fmap (sbt s) $ afb (sa s)
{-# INLINE lens #-}

type Lens' s a = Lens s s a a

newtype Accessor r a = Accessor { runAccessor :: r }

instance Functor (Accessor r) where
  fmap _ (Accessor m) = Accessor m
  {-# INLINE fmap #-}

instance Contravariant (Accessor r) where
  contramap _ (Accessor m) = Accessor m
  {-# INLINE contramap #-}

type Getting r s a = (a -> Accessor r a) -> s -> Accessor r s

infixl 8 ^.

(^.) :: s -> Getting a s a -> a
s ^. l = runAccessor (l Accessor s)
{-# INLINE (^.) #-}

type Setting p s t a b = p a (Mutator b) -> s -> Mutator t

newtype Mutator a = Mutator { runMutator :: a }

instance Functor Mutator where
  fmap f (Mutator a) = Mutator $ f a
  {-# INLINE fmap #-}

over :: Profunctor p => Setting p s t a b -> p a b -> s -> t
over l f = runMutator #. l (Mutator #. f)

type IndexPreservingGetter s a
  = forall p f. (Profunctor p, Contravariant f, Functor f) => p a (f a) -> p s (f s)

to' :: (s -> a) -> IndexPreservingGetter s a
to' f = dimap f coerce
{-# INLINE to' #-}

coerce :: (Contravariant f, Functor f) => f a -> f b
coerce a = fmap absurd $ contramap absurd a

-- The following definition was extracted from the contravariant package.

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

-- The following definitions were extracted from the void package.

newtype Void = Void Void

absurd :: Void -> a
absurd (Void a) = absurd a

-- The following definitions were extracted from the profunctors package.

class Profunctor h where
  lmap :: (a -> b) -> h b c -> h a c
  rmap :: (b -> c) -> h a b -> h a c
  dimap :: (a -> b) -> (c -> d) -> h b c -> h a d
  dimap f g = lmap f . rmap g
  (#.) :: (b -> c) -> h a b -> h a c
  (#.) = \f -> \p -> p `seq` rmap f p

instance Profunctor (->) where
  dimap ab cd bc = cd . bc . ab
  {-# INLINE dimap #-}
  lmap = flip (.)
  {-# INLINE lmap #-}
  rmap = (.)
  {-# INLINE rmap #-}
  (#.) _ = unsafeCoerce
  {-# INLINE (#.) #-}
