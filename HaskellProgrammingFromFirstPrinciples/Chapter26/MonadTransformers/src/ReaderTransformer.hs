module ReaderTransformer where

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
    pure x = ReaderT $ pure . pure $ x

    (ReaderT mf) <*> (ReaderT ma) = ReaderT $ (<*>) <$> mf <*> ma

instance Monad m => Monad (ReaderT r m) where
    return = pure

    (ReaderT ma) >>= f = ReaderT $ \r -> do
        a <- ma r
        runReaderT (f a) r