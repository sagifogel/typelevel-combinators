package typelevel.combinators

import cats.Applicative

trait Sequence[L <: HList] {
  type Out
}

object Sequence {
  type Aux[L <: HList, R] = Sequence[L] { type Out = R }

  def apply[F[_]: Applicative, L <: HList](implicit S: Sequence[L]): Aux[L, S.Out] = S

  implicit def nil[F[_]: Applicative]: Sequence[HNil] = new Sequence[HNil] {
    override type Out = F[HNil]
  }

  implicit def singleton[F[_]: Applicative, A, B]: Aux[F[A] :: HNil, F[A :: HNil]] = new Sequence[F[A] :: HNil] {
    override type Out = F[A :: HNil]
  }

  implicit def inductive[F[_]: Applicative, A, B, T <: HList, R <: HList](
      implicit S: Aux[F[A] :: HNil, F[A :: HNil]],
      S2: Aux[T, F[R]]): Aux[F[A] :: T, F[A :: R]] = new Sequence[F[A] :: T] {
    override type Out = F[A :: R]
  }
}
