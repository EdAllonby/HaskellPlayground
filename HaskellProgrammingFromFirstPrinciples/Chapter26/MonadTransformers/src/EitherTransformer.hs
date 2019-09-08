module EitherTransformer where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (EitherT e m) where
    pure x = EitherT $ pure . pure $ x

    (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

instance (Monad m) => Monad (EitherT e m) where
    return = pure

    (EitherT ma) >>= f = EitherT $ do
        v <- ma
        case v of
            Left e  -> return $ Left e
            Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither (Left a)  = Right a
swapEither (Right e) = Left e

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT e) = EitherT $ fmap swapEither e

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT amc bmc (EitherT e) = do
    t <- e
    case t of
        Left a  -> amc a
        Right e -> bmc e