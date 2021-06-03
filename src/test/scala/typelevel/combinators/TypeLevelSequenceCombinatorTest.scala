package typelevel.combinators

import cats.Applicative
import typelevel.combinators.Sequence.Aux

class TypeLevelSequenceCombinatorTest extends TypeLevelCombinatorSuite {
  def sequenced[F[_]: Applicative, A]: Aux[F[A] :: F[A] :: HNil, F[A :: A :: HNil]] =
    Sequence.apply[F[A] :: F[A] :: HNil]

  test("Sequence - Option[Int] :: Option[Int] :: HNil -> Option[Int :: Int :: HNil") {
    assertResult(show(sequenced[Option, Int], "Sequence")) {
      "TypeTag[[Option[Int] :: Option[Int] :: HNil,Option[Int :: Int :: HNil]]]"
    }
  }
}
