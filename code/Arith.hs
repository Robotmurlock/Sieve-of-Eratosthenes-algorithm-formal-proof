{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  Arith(Nat(..), plus_nat, one_nat, less_nat, Algebraic_semidom,
         Semiring_modulo, Semidom_modulo, Num(..), nat_of_num, dvd, int_to_nat)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Heap;
import qualified Product_Type;
import qualified HOL;

data Nat = Zero_nat | Suc Nat;

int_to_nat :: Integer -> Nat;
int_to_nat 0 = Zero_nat;
int_to_nat x = Suc (int_to_nat (x-1));

nat_to_int :: Nat -> Integer;
nat_to_int Zero_nat = 0;
nat_to_int (Suc x) = 1 + (nat_to_int x);

instance Prelude.Show Nat where {
   show x = Prelude.show $ nat_to_int x 
};

equal_nat :: Nat -> Nat -> Bool;
equal_nat Zero_nat (Suc x2) = False;
equal_nat (Suc x2) Zero_nat = False;
equal_nat (Suc x2) (Suc y2) = equal_nat x2 y2;
equal_nat Zero_nat Zero_nat = True;

instance Eq Nat where {
  a == b = equal_nat a b;
};

plus_nat :: Nat -> Nat -> Nat;
plus_nat (Suc m) n = plus_nat m (Suc n);
plus_nat Zero_nat n = n;

times_nat :: Nat -> Nat -> Nat;
times_nat Zero_nat n = Zero_nat;
times_nat (Suc m) n = plus_nat n (times_nat m n);

class Times a where {
  times :: a -> a -> a;
};

class (Times a) => Dvd a where {
};

instance Times Nat where {
  times = times_nat;
};

instance Dvd Nat where {
};

one_nat :: Nat;
one_nat = Suc Zero_nat;

class One a where {
  one :: a;
};

instance One Nat where {
  one = one_nat;
};

class Plus a where {
  plus :: a -> a -> a;
};

instance Plus Nat where {
  plus = plus_nat;
};

class Zero a where {
  zero :: a;
};

instance Zero Nat where {
  zero = Zero_nat;
};

class (Plus a) => Semigroup_add a where {
};

class (One a, Semigroup_add a) => Numeral a where {
};

instance Semigroup_add Nat where {
};

instance Numeral Nat where {
};

class (One a, Times a) => Power a where {
};

instance Power Nat where {
};

minus_nat :: Nat -> Nat -> Nat;
minus_nat (Suc m) (Suc n) = minus_nat m n;
minus_nat Zero_nat n = Zero_nat;
minus_nat m Zero_nat = m;

class Minus a where {
  minus :: a -> a -> a;
};

instance Minus Nat where {
  minus = minus_nat;
};

less_nat :: Nat -> Nat -> Bool;
less_nat m (Suc n) = less_eq_nat m n;
less_nat n Zero_nat = False;

less_eq_nat :: Nat -> Nat -> Bool;
less_eq_nat (Suc m) n = less_nat m n;
less_eq_nat Zero_nat n = True;

divmod_nat :: Nat -> Nat -> (Nat, Nat);
divmod_nat m n =
  (if equal_nat n Zero_nat || less_nat m n then (Zero_nat, m)
    else let {
           a = divmod_nat (minus_nat m n) n;
         } in (case a of {
                (q, aa) -> (Suc q, aa);
              }));

divide_nat :: Nat -> Nat -> Nat;
divide_nat m n = fst (divmod_nat m n);

class Divide a where {
  divide :: a -> a -> a;
};

instance Divide Nat where {
  divide = divide_nat;
};

modulo_nat :: Nat -> Nat -> Nat;
modulo_nat m n = snd (divmod_nat m n);

class (Divide a, Dvd a) => Modulo a where {
  modulo :: a -> a -> a;
};

instance Modulo Nat where {
  modulo = modulo_nat;
};

class (Semigroup_add a) => Ab_semigroup_add a where {
};

class (Semigroup_add a, Zero a) => Monoid_add a where {
};

class (Ab_semigroup_add a, Monoid_add a) => Comm_monoid_add a where {
};

class (Times a, Zero a) => Mult_zero a where {
};

class (Times a) => Semigroup_mult a where {
};

class (Ab_semigroup_add a, Semigroup_mult a) => Semiring a where {
};

class (Comm_monoid_add a, Mult_zero a, Semiring a) => Semiring_0 a where {
};

class (Semiring_0 a) => Semiring_no_zero_divisors a where {
};

class (Semigroup_mult a, Power a) => Monoid_mult a where {
};

class (Monoid_mult a, Numeral a, Semiring a) => Semiring_numeral a where {
};

class (One a, Zero a) => Zero_neq_one a where {
};

class (Semiring_numeral a, Semiring_0 a, Zero_neq_one a) => Semiring_1 a where {
};

class (Semiring_1 a,
        Semiring_no_zero_divisors a) => Semiring_1_no_zero_divisors a where {
};

class (Semigroup_add a) => Cancel_semigroup_add a where {
};

class (Ab_semigroup_add a, Cancel_semigroup_add a,
        Minus a) => Cancel_ab_semigroup_add a where {
};

class (Cancel_ab_semigroup_add a,
        Comm_monoid_add a) => Cancel_comm_monoid_add a where {
};

class (Cancel_comm_monoid_add a, Semiring_0 a) => Semiring_0_cancel a where {
};

class (Semigroup_mult a) => Ab_semigroup_mult a where {
};

class (Ab_semigroup_mult a, Semiring a) => Comm_semiring a where {
};

class (Comm_semiring a, Semiring_0 a) => Comm_semiring_0 a where {
};

class (Comm_semiring_0 a,
        Semiring_0_cancel a) => Comm_semiring_0_cancel a where {
};

class (Semiring_0_cancel a, Semiring_1 a) => Semiring_1_cancel a where {
};

class (Ab_semigroup_mult a, Monoid_mult a, Dvd a) => Comm_monoid_mult a where {
};

class (Comm_monoid_mult a, Comm_semiring_0 a,
        Semiring_1 a) => Comm_semiring_1 a where {
};

class (Comm_semiring_0_cancel a, Comm_semiring_1 a,
        Semiring_1_cancel a) => Comm_semiring_1_cancel a where {
};

class (Comm_semiring_1_cancel a,
        Semiring_1_no_zero_divisors a) => Semidom a where {
};

instance Ab_semigroup_add Nat where {
};

instance Monoid_add Nat where {
};

instance Comm_monoid_add Nat where {
};

instance Mult_zero Nat where {
};

instance Semigroup_mult Nat where {
};

instance Semiring Nat where {
};

instance Semiring_0 Nat where {
};

instance Semiring_no_zero_divisors Nat where {
};

instance Monoid_mult Nat where {
};

instance Semiring_numeral Nat where {
};

instance Zero_neq_one Nat where {
};

instance Semiring_1 Nat where {
};

instance Semiring_1_no_zero_divisors Nat where {
};

instance Cancel_semigroup_add Nat where {
};

instance Cancel_ab_semigroup_add Nat where {
};

instance Cancel_comm_monoid_add Nat where {
};

instance Semiring_0_cancel Nat where {
};

instance Ab_semigroup_mult Nat where {
};

instance Comm_semiring Nat where {
};

instance Comm_semiring_0 Nat where {
};

instance Comm_semiring_0_cancel Nat where {
};

instance Semiring_1_cancel Nat where {
};

instance Comm_monoid_mult Nat where {
};

instance Comm_semiring_1 Nat where {
};

instance Comm_semiring_1_cancel Nat where {
};

instance Semidom Nat where {
};

class (Semiring_no_zero_divisors a) => Semiring_no_zero_divisors_cancel a where {
};

class (Divide a, Semidom a,
        Semiring_no_zero_divisors_cancel a) => Semidom_divide a where {
};

instance Semiring_no_zero_divisors_cancel Nat where {
};

instance Semidom_divide Nat where {
};

class (Semidom_divide a) => Algebraic_semidom a where {
};

class (Comm_semiring_1_cancel a, Modulo a) => Semiring_modulo a where {
};

class (Algebraic_semidom a, Semiring_modulo a) => Semidom_modulo a where {
};

instance Algebraic_semidom Nat where {
};

instance Semiring_modulo Nat where {
};

instance Semidom_modulo Nat where {
};

data Num = One | Bit0 Num | Bit1 Num;

nat_of_num :: Num -> Nat;
nat_of_num (Bit1 n) = let {
                        m = nat_of_num n;
                      } in Suc (plus_nat m m);
nat_of_num (Bit0 n) = let {
                        m = nat_of_num n;
                      } in plus_nat m m;
nat_of_num One = one_nat;

dvd :: forall a. (Eq a, Semidom_modulo a) => a -> a -> Bool;
dvd a b = modulo b a == zero;

}
