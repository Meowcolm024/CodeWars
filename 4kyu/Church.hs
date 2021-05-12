{-# LANGUAGE RankNTypes #-}

module Church where

import Prelude hiding (succ)

newtype Number = Nr (forall a. (a -> a) -> a -> a)

zero :: Number
zero = Nr (\ _ z -> z)

succ :: Number -> Number
succ (Nr a) = Nr (\ s z -> s (a s z))

one :: Number
one = succ zero

add :: Number -> Number -> Number
add (Nr a) (Nr b) = Nr $ a (\n f x -> f (n f x)) b

mult :: Number -> Number -> Number
mult (Nr a) (Nr b) = Nr $ \f -> a (b f)

pow :: Number -> Number -> Number
pow (Nr x) (Nr n) = Nr $ n x