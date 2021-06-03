package typelevel.combinators

import cats.Eval

trait FoldRight[L <: HList] {
  type Out
}

object FoldRight {
  type Aux[L <: HList, R] = FoldRight[L] { type Out = R }

  def apply[L <: HList](implicit F: FoldRight[L]): Aux[L, F.Out] = F

  implicit def nil[B](implicit Z: Val[Eval[B]]): Aux[HNil, Eval[B]] = new FoldRight[HNil] {
    override type Out = Eval[B]
  }

  implicit def singleton[A, B](implicit Z: Val[Eval[B]], F: (A, Eval[B]) -> Eval[B]): Aux[A :: HNil, Eval[B]] =
    new FoldRight[A :: HNil] {
      override type Out = Eval[B]
    }

  implicit def inductive[A, B, L <: HList](
      implicit Z: Val[Eval[B]],
      F: (A, Eval[B]) -> Eval[B],
      FM: Aux[A :: HNil, Eval[B]],
      FM2: Aux[L, Eval[B]]): Aux[A :: L, Eval[B]] =
    new FoldRight[A :: L] {
      override type Out = Eval[B]
    }
}
