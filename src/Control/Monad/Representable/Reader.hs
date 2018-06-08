{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Representable.Reader
-- Copyright   :  (c) Edward Kmett 2011,
--                (c) Conal Elliott 2008
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
--
-- Representable functors on Hask are all monads, because they are isomorphic to
-- a 'Reader' monad.
----------------------------------------------------------------------

module Control.Monad.Representable.Reader
  (
  -- * Representable functor monad
    Reader
  , runReader
  -- * Monad Transformer
  , ReaderT(..), readerT, runReaderT
  , MonadReader(..)
  , module Data.Functor.Rep
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Comonad
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class as Writer
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Distributive
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
#endif
import Data.Functor.Semimonad
import Data.Functor.Extend
import Data.Functor.Identity
import Data.Functor.Rep
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Semigroup.Semifoldable
import Data.Semigroup.Semitraversable
import GHC.Generics hiding (Rep)

type Reader f = ReaderT f Identity

runReader :: Representable f => Reader f b -> Rep f -> b
runReader = fmap runIdentity . runReaderT

-- * This 'representable monad transformer' transforms any monad @m@ with a 'Representable' 'Monad'.
--   This monad in turn is also representable if @m@ is 'Representable'.
newtype ReaderT f m b = ReaderT { getReaderT :: f (m b) }

readerT :: Representable f => (Rep f -> m b) -> ReaderT f m b
readerT = ReaderT . tabulate

runReaderT :: Representable f => ReaderT f m b -> Rep f -> m b
runReaderT = index . getReaderT

instance (Functor f, Functor m) => Functor (ReaderT f m) where
  fmap f = ReaderT . fmap (fmap f) . getReaderT

instance (Representable f, Representable m) => Representable (ReaderT f m) where
  type Rep (ReaderT f m) = (Rep f, Rep m)
  tabulate = ReaderT . tabulate . fmap tabulate . curry
  index = uncurry . fmap index . index . getReaderT
  cosemitraverse = cosemitraverseIso (Comp1 . getReaderT) (ReaderT . unComp1)

instance (Representable f, Semiapplicative m) => Semiapplicative (ReaderT f m) where
  ReaderT ff <.> ReaderT fa = ReaderT (unCo ((<.>) <$> Co ff <.> Co fa))

instance (Representable f, Applicative m) => Applicative (ReaderT f m) where
  pure = ReaderT . pureRep . pure
  ReaderT ff <*> ReaderT fa = ReaderT (unCo ((<*>) <$> Co ff <*> Co fa))

instance (Representable f, Semimonad m) => Semimonad (ReaderT f m) where
  ReaderT fm >>- f = ReaderT $ mzipWithRep (>>-) fm $ distribute (getReaderT . f)

instance (Representable f, Monad m) => Monad (ReaderT f m) where
#if __GLASGOW_HASKELL__ < 710
  return = ReaderT . pureRep . return
#endif
  ReaderT fm >>= f = ReaderT $ mzipWithRep (>>=) fm $ distribute (getReaderT . f)

#if __GLASGOW_HASKELL >= 704

instance (Representable f, Monad m, Rep f ~ e) => MonadReader e (ReaderT f m) where
  ask = ReaderT (tabulate return)
  local f m = readerT $ \r -> runReaderT m (f r)
#if MIN_VERSION_transformers(0,3,0)
  reader = readerT . fmap return
#endif

#endif

instance Representable f => MonadTrans (ReaderT f) where
  lift = ReaderT . pureRep

instance (Representable f, Distributive m) => Distributive (ReaderT f m) where
  distribute = ReaderT . fmap distribute . unCo . collect (Co . getReaderT)

instance (Representable f, Representable m, Semigroup (Rep f), Semigroup (Rep m)) => Extend (ReaderT f m) where
  extended = extendedRep
  duplicated = duplicatedRep

instance (Representable f, Representable m, Monoid (Rep f), Monoid (Rep m)) => Comonad (ReaderT f m) where
  extend = extendRep
  duplicate = duplicateRep
  extract = extractRep

instance (Representable f, MonadIO m) => MonadIO (ReaderT f m) where
  liftIO = lift . liftIO

instance (Representable f, MonadWriter w m) => MonadWriter w (ReaderT f m) where
  tell = lift . tell
  listen = ReaderT . fmap listen . getReaderT
  pass = ReaderT . fmap pass . getReaderT

-- misc. instances that can exist, but aren't particularly about representability

instance (Foldable f, Foldable m) => Foldable (ReaderT f m) where
  foldMap f = foldMap (foldMap f) . getReaderT

instance (Semifoldable f, Semifoldable m) => Semifoldable (ReaderT f m) where
  semifoldMap f = semifoldMap (semifoldMap f) . getReaderT

instance (Traversable f, Traversable m) => Traversable (ReaderT f m) where
  traverse f = fmap ReaderT . traverse (traverse f) . getReaderT

instance (Semitraversable f, Semitraversable m) => Semitraversable (ReaderT f m) where
  semitraverse f = fmap ReaderT . semitraverse (semitraverse f) . getReaderT
