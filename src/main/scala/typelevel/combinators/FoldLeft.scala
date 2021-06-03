package typelevel.combinators

trait FoldLeft[L <: HList] {
  type Out
}

object FoldLeft {
  type Aux[L <: HList, B] = FoldLeft[L] { type Out = B }

  def apply[L <: HList](implicit F: FoldLeft[L]): Aux[L, F.Out] = F

  implicit def nil[B](implicit Z: Val[B]): Aux[HNil, B] = new FoldLeft[HNil] {
    override type Out = B
  }

  implicit def singleton[A, B](implicit Z: Val[B], F: (B, A) -> B): Aux[A :: HNil, B] = new FoldLeft[A :: HNil] {
    override type Out = B
  }

  implicit def inductive[A, B, L <: HList](
      implicit Z: Val[B],
      F: (B, A) -> B,
      FM: Aux[A :: HNil, B],
      FM2: Aux[L, B]): Aux[A :: L, B] =
    new FoldLeft[A :: L] {
      override type Out = B
    }
}
