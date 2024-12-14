module Identity (
  Identity(..)
) where

newtype Identity a = Identity {
    unIdentity :: a
  }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity b) = Identity (f b)

instance Monad Identity where
  (Identity a) >>= f = f a
