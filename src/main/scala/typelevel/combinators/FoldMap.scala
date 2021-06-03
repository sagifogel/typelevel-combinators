package typelevel.combinators

import cats.Monoid

/** type level foldMap function representation
  *
  * @tparam L HList
  */
trait FoldMap[L <: HList] {
  type Out
}

object FoldMap {
  type Aux[L <: HList, R] = FoldMap[L] { type Out = R }

  def apply[L <: HList](implicit F: FoldMap[L]): Aux[L, F.Out] = F

  implicit def nil[R: Monoid]: Aux[HNil, R] = new FoldMap[HNil] {
    override type Out = R
  }

  implicit def singleton[R: Monoid, A](implicit F: A -> R): Aux[A :: HNil, R] = new FoldMap[A :: HNil] {
    override type Out = R
  }

  implicit def inductive[R: Monoid, A, L <: HList](
      implicit F: A -> R,
      FM: Aux[A :: HNil, R],
      FM2: Aux[L, R]): Aux[A :: L, R] =
    new FoldMap[A :: L] {
      override type Out = R
    }
}
