package typelevel.combinators

import typelevel.combinators.Nat._1

trait ReplicateM[N <: Nat] {
  type Out
}

object ReplicateM {
  type Aux[N <: Nat, R] = ReplicateM[N] { type Out = R }

  def apply[N <: Nat](implicit R: ReplicateM[N]): Aux[N, R.Out] = R

  implicit def nil[F[_], A](implicit F: Val[F[A]]): Aux[_0, F[HNil]] = new ReplicateM[_0] {
    override type Out = F[HNil]
  }

  implicit def singleton[F[_], A](implicit F: Val[F[A]]): Aux[_1, F[A :: HNil]] =
    new ReplicateM[_1] {
      override type Out = F[A :: HNil]
    }

  implicit def inductive[F[_], A, N1 <: Nat, L <: HList, R <: HList](
      implicit F: Val[F[A]],
      T: Aux[N1, F[L]]): Aux[Succ[N1], F[A :: L]] =
    new ReplicateM[Succ[N1]] {
      override type Out = F[A :: L]
    }
}
