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
