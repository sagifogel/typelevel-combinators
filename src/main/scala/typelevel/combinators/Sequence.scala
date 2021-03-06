package typelevel.combinators

import cats.Applicative

/** type level sequence function representation
  *
  * @tparam L HList
  */
trait Sequence[L <: HList] {
  type Out
}

object Sequence {
  type Aux[L <: HList, R] = Sequence[L] { type Out = R }

  def apply[L <: HList](implicit S: Sequence[L]): Aux[L, S.Out] = S

  implicit def nil[F[_]: Applicative]: Aux[HNil, F[HNil]] = new Sequence[HNil] {
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
