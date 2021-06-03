package typelevel.combinators

import typelevel.combinators.->._
import typelevel.combinators.FoldLeft.Aux

class TypeLevelFoldLeftCombinatorTest extends TypeLevelCombinatorSuite {
  def foldLeft[A, B](implicit Z: Val[B], F: (B, A) -> B): Aux[A :: A :: HNil, B] =
    FoldLeft.apply[A :: A :: HNil]

  test("FoldLeft - Option[Int] :: Option[Int] :: HNil -> Int -> ((Int, Option[Int]) -> Int) -> Int") {
    assertResult(show(foldLeft[Option[Int], Int], "FoldLeft")) {
      "TypeTag[[Option[Int] :: Option[Int] :: HNil,Int]]"
    }
  }
}
