package typelevel.combinators

import cats.Applicative

trait Traverse[L <: HList] {
  type Out
}

object Traverse {
  type Aux[L <: HList, R] = Traverse[L] { type Out = R }

  def apply[L <: HList](implicit T: Traverse[L]): Aux[L, T.Out] = T

  implicit def nil[F[_]: Applicative]: Aux[HNil, F[HNil]] = new Traverse[HNil] {
    override type Out = F[HNil]
  }

  implicit def singleton[F[_]: Applicative, A, B](implicit F: A -> F[B]): Aux[A :: HNil, F[B :: HNil]] = new Traverse[A :: HNil] {
    override type Out = F[B :: HNil]
  }

  implicit def inductive[F[_]: Applicative, A, B, T <: HList, R <: HList](
      implicit F: A -> F[B],
      T: Aux[A :: HNil, F[B :: HNil]],
      T2: Aux[T, F[R]]): Aux[A :: T, F[B :: R]] =
    new Traverse[A :: T] {
      override type Out = F[B :: R]
    }
}
