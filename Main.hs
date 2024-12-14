import Identity

type Cont r a = ContT r Identity a

newtype ContT r m a = ContT {
    runContT :: (a -> m r) -> m r
  }

evalContT :: Monad m => ContT a m a -> m a
evalContT (ContT c) = c return


instance Functor (ContT r m) where
  fmap :: (a -> b) -> ContT r m a -> ContT r m b
  fmap f m = ContT $ \c -> runContT m (c . f)

instance Applicative (ContT r m) where
  pure a = ContT $ \c -> c a
  f <*> a = ContT $ \c -> runContT f $ \g -> runContT a (c . g)

instance Monad (ContT r m) where
  (>>=) :: ContT r m a -> (a -> ContT r m b) -> ContT r m b
  a >>= f = ContT $ \c -> runContT a $ \a' -> runContT (f a') c


main :: IO ()
main = do
  let c = ContT $ return . const "Hello, World!"
  msg <- evalContT c
  putStrLn msg

  let c' = ContT $ return . const "Hello, World!"
      msg' = unIdentity . evalContT $ c'
  putStrLn msg'

  return ()
