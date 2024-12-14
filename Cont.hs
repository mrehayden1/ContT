toCPS :: a -> (a -> r) -> r
toCPS a f = f a

fromCPS :: (forall r. (a -> r) -> r) -> a
fromCPS cps = cps id


succCPS :: Num a => a -> (a -> r) -> r
succCPS = flip ($) . (+ 1)

succSuccFive :: Num a => a
succSuccFive = succCPS 5 $ \s5 ->
                 succCPS s5 id

foo :: Num a => a
foo = succCPS 4 $ \s4 ->
        succCPS 5 $ \s5 ->
          succCPS 6 $ \s6 ->
            s4 + s5 + s6


newtype Cont r a = Cont {
    runCont :: (a -> r) -> r
  }

unCont :: (forall r. Cont r a) -> a
unCont (Cont c) = c id


instance Functor (Cont r) where
  fmap f c = Cont $ \c' -> runCont c (c' . f)

instance Applicative (Cont r) where
  pure a = Cont $ \c -> c a
  f <*> a = Cont $ \cb ->
              runCont f $ \f' ->
                runCont a $ \a' ->
                  cb (f' a')

instance Monad (Cont r) where
  ma >>= f = Cont $ \cb ->
               runCont ma $ \a ->
                 runCont (f a) cb

succCont :: Num a => a -> Cont r a
succCont a = Cont $ \c -> c (a + 1)

bar :: Num a => a
bar = flip runCont id
        $ (\a b c -> a + b + c) <$> succCont 4 <*> succCont 5 <*> succCont 6

barM :: Num a => a
barM = flip runCont id $ do
  s4 <- succCont 4
  s5 <- succCont 5
  s6 <- succCont 6
  return $ s4 + s5 + s6
