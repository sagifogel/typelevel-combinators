package typelevel.combinators

sealed trait HList
final class ::[+H, +T <: HList] extends HList
final class HNil extends HList
