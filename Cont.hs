newtype Cont r a = Cont {
    runCont :: (a -> r) -> r
  }

instance Functor (Cont r) where
  fmap f c = Cont $ \c' -> runCont c (c' . f)

instance Applicative (Cont r) where
  pure a = Cont $ \c -> c a
  f <*> a = Cont $ \cb ->
              runCont f $ \f' ->
                runCont a $ \a' ->
                  cb (f' a')

instance Monad (Cont r) where
  ma >>= f = Cont $ \c ->
               runCont ma $ \a ->
                 runCont (f a) c


callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \c -> flip runCont c . f $ Cont . const . c


succCont :: Num a => a -> Cont r a
succCont a = Cont $ \c -> c (a + 1)

foo :: Num a => a
foo = flip runCont id
        $ (\a b c -> a + b + c) <$> succCont 4 <*> succCont 5 <*> succCont 6

bar :: Num a => a
bar = flip runCont id $ do
  s4 <- succCont 4
  s5 <- succCont 5
  s6 <- succCont 6
  return $ s4 + s5 + s6

baz :: Bool -> String -> Cont r String
baz flag name = do
  callCC (\cc -> do
    msg <- addGreeting flag name cc
    pure ("Result: " <> msg))

addGreeting :: Bool -> String -> (String -> Cont r String) -> Cont r String
addGreeting flag name k = do
  if flag then k "Flag is set, exiting early"
  else pure ("Hello, " <> name <> "!")
