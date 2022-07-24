module Day25 where

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
-- fmap :: (a -> b) -> MaybeT a -> MaybeT b
    fmap f x = MaybeT $ fmap (fmap f) $ runMaybeT x

instance Monad m => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  MaybeT f <*> MaybeT x = MaybeT $ (<*>) <$> f <*> x