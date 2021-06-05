package typelevel.combinators

sealed trait Nat
class _0 extends Nat
class Succ[A <: Nat] extends Nat

object Nat {
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
}
