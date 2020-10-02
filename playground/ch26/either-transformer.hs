{-# LANGUAGE InstanceSigs #-}

module EitherTransformer where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
    fmap f (EitherT x) = EitherT $ (fmap . fmap) f x

instance (Applicative m) => Applicative (EitherT e m) where
    pure x = EitherT $ (pure . pure) x
    (<*>) (EitherT f) (EitherT x) = EitherT $ (<*>) <$> f <*> x

instance (Monad m) => Monad (EitherT e m) where
    return = pure
    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (EitherT x) >>= f =
        EitherT $ do
            v <- x
            case v of
                Left y -> return $ Left y
                Right z -> runEitherT (f z)