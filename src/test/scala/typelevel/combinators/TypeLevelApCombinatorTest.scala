package typelevel.combinators

import typelevel.combinators.Ap.Aux

class TypeLevelApCombinatorTest extends TypeLevelCombinatorSuite {
  def ap[A, B]: Aux[(A -> B) :: (A -> B) :: HNil, A :: A :: HNil, B :: B :: HNil] =
    Ap.apply[(A -> B) :: (A -> B) :: HNil, A :: A :: HNil]

  test("Ap - (Int -> String) :: (Int -> String) :: HNil -> A :: A :: HNil -> String :: String :: HNil") {
    assertResult(show(ap[Int, String], "Ap")) {
      "TypeTag[[(Int -> String) :: (Int -> String) :: HNil,Int :: Int :: HNil,String :: String :: HNil]]"
    }
  }
}
