package typelevel.combinators

import cats.Eval
import typelevel.combinators.FoldRight.Aux

class TypeLevelFoldRightCombinatorTest extends TypeLevelCombinatorSuite {
  def foldRight[A, B](implicit Z: Val[Eval[B]], F: (A, Eval[B]) -> Eval[B]): Aux[A :: A :: HNil, Eval[B]] =
    FoldRight.apply[A :: A :: HNil]

  test("FoldRight - Int :: Int :: HNil -> Eval[Int] -> ((Int, Eval[Int]) -> Eval[Int]) -> Eval[Int]") {
    assertResult(show(foldRight[Int, Int], "FoldRight")) {
      "TypeTag[[Int :: Int :: HNil,cats.Eval[Int]]]"
    }
  }
}
