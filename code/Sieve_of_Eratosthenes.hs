{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Sieve_of_Eratosthenes(erast) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Heap;
import qualified HOL;
import qualified List;
import qualified Arith;

remove_multipliers :: [Arith.Nat] -> Arith.Nat -> [Arith.Nat];
remove_multipliers [] n = [];
remove_multipliers (x : xs) n =
  (if Arith.dvd n x && Arith.less_nat n x then remove_multipliers xs n
    else x : remove_multipliers xs n);

erasta :: [Arith.Nat] -> [Arith.Nat] -> [Arith.Nat];
erasta [] ys = ys;
erasta (x : xs) ys =
  let {
    es = erasta xs ys;
  } in (if List.member es x then remove_multipliers es x else es);

nlist :: Arith.Nat -> [Arith.Nat];
nlist n =
  reverse
    (List.upt (Arith.nat_of_num (Arith.Bit0 Arith.One))
      (Arith.plus_nat n Arith.one_nat));

erast :: Arith.Nat -> [Arith.Nat];
erast n = reverse (erasta (nlist n) (nlist n));

}
