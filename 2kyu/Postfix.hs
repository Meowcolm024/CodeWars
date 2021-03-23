-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE RankNTypes #-}

module Postfix where

-- data Begin
-- data Add
-- data Push
-- data End

-- class Language f r where
--     exec :: f -> r

-- instance Language (f, Int) r => Language ((f, Int), Int) (Add -> r) where
--     exec ((v, n), m) _ = exec (v, n + m)

-- instance (a ~ Int, Language (f, Int) r) => Language f (Push -> a -> r) where
--     exec v _ n = exec (v, n)

-- instance a ~ Int => Language (v, Int) (End -> a) where
--     exec (_, n) _ = n

-- begin :: forall r . Language Begin r => r
-- begin = exec (undefined :: Begin)

-- add :: Add
-- add = undefined

-- push :: Push
-- push = undefined

-- end :: End
-- end = undefined

begin f = f []
push st n f = f (n:st)
add (x:y:st) f = f (x+y:st)
end = head