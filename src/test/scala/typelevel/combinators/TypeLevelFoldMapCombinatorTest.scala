package typelevel.combinators

import cats.Monoid
import typelevel.combinators.->._
import typelevel.combinators.FoldMap.Aux

class TypeLevelFoldMapCombinatorTest extends TypeLevelCombinatorSuite {
  def foldMapped[R: Monoid, A]: Aux[A :: A :: HNil, R] =
    FoldMap.apply[A :: A :: HNil]

  test("FoldMap [Int :: Int :: HNil] -> (Int -> Int) -> Int]") {
    assertResult(show(foldMapped[Int, Int], "FoldMap")) {
      "TypeTag[[Int :: Int :: HNil,Int]]"
    }
  }
}
