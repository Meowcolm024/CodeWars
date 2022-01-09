{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module AdditionCommutes ( plusCommutes ) where

-- import Kata.AdditionCommutes.Definitions
--   ( Z, S
--   , Natural(..), Equal(..)
--   , (:+:))

-- These are some lemmas that may be helpful.
-- They will *not* be tested, so rename them
-- if you so desire. Good luck!

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS n) = EqlS (symmetric n)

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS n) (EqlS m) = EqlS (transitive n m)

plusZero :: Natural a -> Equal (a :+: Z) (Z :+: a)
plusZero NumZ     = EqlZ
plusZero (NumS n) = EqlS (plusZero n)

simpl :: Natural a -> Natural b -> Equal (b :+: S a) (S (b :+: a))
simpl NumZ NumZ     = EqlS EqlZ
simpl (NumS n) NumZ = EqlS (simpl n NumZ)
simpl n (NumS m)    = EqlS (simpl n m)

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes n NumZ = plusZero n
plusCommutes n (NumS m) = (simpl m n) `transitive` EqlS (plusCommutes n m)

-- For reference, here are the definitions, if you
-- want to copy them into an IDE:
-- {-

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
    NumZ :: Natural Z
    NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- -}
