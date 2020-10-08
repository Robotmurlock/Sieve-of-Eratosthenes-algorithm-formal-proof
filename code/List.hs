{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module List(upt, member) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Heap;
import qualified Arith;

upt :: Arith.Nat -> Arith.Nat -> [Arith.Nat];
upt i j = (if Arith.less_nat i j then i : upt (Arith.Suc i) j else []);

member :: forall a. (Eq a) => [a] -> a -> Bool;
member [] y = False;
member (x : xs) y = x == y || member xs y;

}
