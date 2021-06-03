package typelevel.combinators

trait Ap[F <: HList, L <: HList] {
  type Out
}

object Ap {
  type Aux[F <: HList, L <: HList, R] = Ap[F, L] { type Out = R }

  def apply[F <: HList, L <: HList](implicit T: Ap[F, L]): Aux[F, L, T.Out] = T

  implicit def nil: Aux[HNil, HNil, HNil] = new Ap[HNil, HNil] {
    override type Out = HNil
  }

  implicit def singleton[A, B]: Aux[(A -> B) :: HNil, A :: HNil, B :: HNil] = new Ap[(A -> B) :: HNil, A :: HNil] {
    override type Out = B :: HNil
  }

  implicit def inductive[A, B, F <: HList, L <: HList, R <: HList](
      implicit S: Aux[(A -> B) :: HNil, A :: HNil, B :: HNil],
      S2: Aux[F, L, R]): Aux[(A -> B) :: F, A :: L, B :: R] = new Ap[(A -> B) :: F, A :: L] {
    override type Out = B :: R
  }
}
