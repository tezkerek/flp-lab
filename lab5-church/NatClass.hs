{-# LANGUAGE ScopedTypeVariables #-}

module NatClass where

import Prelude (Show(..), (<>), Num(fromInteger), undefined)
import qualified GHC.Natural as Natural

import MyPrelude
import BoolClass
import MaybeClass
import PairClass
 
-- | The class of Natural-like types (types having a notion of
-- 'zero', 'succ'essor, and 'iter'ation).
-- Instances should satisfy the following:
--
-- [Zero Iterations] @'iter' f i 'zero' = i@
-- [Successor Iterations]  @'iter' f i ('succ' n) = f ('iter' f i n)@
class NatClass n where
  zero :: n
  succ :: n -> n
  iter :: (a -> a) -> a -> n -> a

instance NatClass Natural.Natural where
  zero = 0
  succ n = n `Natural.plusNatural` 1
  iter f i 0 = i
  iter f i n = f (iter f i (n `Natural.minusNatural` 1))

one :: NatClass n => n
one = succ zero

-- >>> one :: Natural.Natural
-- 1

-- | The isZero function returns 'true' iff its argument is 'zero'.
isZero :: NatClass n => n -> CBool
isZero = iter (const false) true

-- >>> isZero (one :: Natural.Natural)
-- CFalse

-- | Usual natural numbers addition
add :: NatClass n => n -> n -> n
add n m = iter succ n m

-- >>> add one one :: Natural.Natural
-- 2

-- | Usual natural numbers multiplication
mul :: NatClass n => n -> n -> n
mul n m = iter (add n) zero m

-- >>> mul one one :: Natural.Natural
-- 1

-- | Usual natural numbers exponentiation (@exp m n@ is @m ^ n@)
exp :: NatClass n => n -> n -> n
exp n p = iter (mul n) one p

-- >>> exp (add one one) (add one one) :: Natural.Natural
-- 4

-- | Predecessor of a natural number ('nothing' for 'zero')
pred :: NatClass n  => n -> CMaybe n
pred = iter (just . maybe zero succ) nothing

-- >>> pred zero :: CMaybe Natural.Natural
-- CNothing

-- | Difference between natural numbers as a 'MaybeClass' ('nothing' if first is smaller)
sub :: NatClass n  => n -> n -> CMaybe n
sub n m = iter (maybeBind pred) (just n) m

-- >>> sub (exp (add one one) (add one one)) one :: CMaybe Natural.Natural
-- CJust 3

-- | (Strictly-)Less-Than predicate for natural numbers.
lt :: NatClass n => n -> n -> CBool 
lt n m = maybe true (const false) (sub n m)

-- >>> lt (one :: Natural.Natural) one
-- CFalse

-- | (Strictly-)Greater-Than predicate for natural numbers.
gt :: NatClass n => n -> n -> CBool 
gt n m = maybe true (const false) (sub m n)

-- >>> gt (one :: Natural.Natural) one
-- CFalse

-- | Greater-Than-or-Equal-To predicate for natural numbers.
gte :: NatClass n => n -> n -> CBool 
gte n m = not (lt n m)

-- >>> gte (zero :: Natural.Natural) zero
-- CTrue

-- | Less-Than-or-Equal-To predicate for natural numbers.
lte :: NatClass n => n -> n -> CBool 
lte n m = not (gt n m)

-- >>> lte (zero :: Natural.Natural) zero
-- CTrue

-- | Equality predicate for natural numbers.
eq :: NatClass n => n -> n -> CBool 
eq n m = not (lt n m) && not (gt n m)

-- >>> eq (zero :: Natural.Natural) zero
-- CTrue

-- >>> eq (zero :: Natural.Natural) one
-- CFalse

-- | Returns the greater between its two arguments
max :: NatClass n => n -> n -> n
max n m = bool m n (gt n m)

-- >>> max (zero :: Natural.Natural) one
-- 1

newtype CNat = CNat { getCNat :: forall a . (a -> a) -> a -> a }

instance NatClass CNat where
  iter f i n = getCNat n f i
  zero = CNat (const id)
  succ n = CNat (\f x -> f (getCNat n f x))

-- | converting between different instances of 'NatClass'
fromNatClass :: (NatClass n, NatClass m) => n -> m
fromNatClass = iter succ zero

-- | 'Show' instance for 'CNat' (via transformation into Haskell Natural)
instance Show CNat where
  show cn = "C" <> show (fromNatClass cn :: Natural.Natural)

-- | to allow recognizing natural numbers as 'CNat'
instance Num CNat where
  fromInteger n = fromNatClass (fromInteger n :: Natural.Natural)

-- >>> 1 :: CNat
-- C1

-- >>> isZero (1 :: CNat)
-- CFalse

-- >>> add 3 5 :: CNat
-- C8

-- >>> mul 2 7 :: CNat
-- C14

-- >>> exp 2 10 :: CNat
-- C1024

-- >>> pred 0 :: CMaybe CNat
-- CNothing

-- >>> sub 5 5 :: CMaybe CNat
-- CJust C0

-- >>> lt 7 (7 :: CNat)
-- CFalse

-- >>> gt (one :: CNat) one
-- CFalse

-- >>> gte (zero :: CNat) zero
-- CTrue

-- >>> lte (zero :: CNat) zero
-- CTrue

-- >>> eq (zero :: CNat) zero
-- CTrue

-- >>> eq (zero :: CNat) one
-- CFalse

-- >>> max (zero :: CNat) one
-- C1
