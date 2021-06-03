package typelevel.combinators

import typelevel.combinators.ZipWithIndex.Aux

class TypeLevelZipWithIndexTest extends TypeLevelCombinatorSuite {
  def zipWithIndex[A]: Aux[A :: A :: HNil, (A, Int) :: (A, Int) :: HNil] =
    ZipWithIndex.apply[A :: A :: HNil]

  test("ZipWithIndex - String :: String :: HNil -> (String, Int) :: (String, Int) :: HNil") {
    assertResult(show(zipWithIndex[String], "ZipWithIndex")) {
      "TypeTag[[String :: String :: HNil,(String, Int) :: (String, Int) :: HNil]]"
    }
  }
}
