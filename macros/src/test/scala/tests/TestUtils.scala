package tests

import utest.AssertionError

import scala.meta._

object TestUtils {
  def structurallyEqual(actual: Tree, expected: Tree): Int =
    scala.meta.testkit.StructurallyEqual(actual, expected) match {
      case Left(diff) =>
        println(actual.syntax)
        throw AssertionError(diff.detailed, Nil)
      case Right(()) => 0
    }

  def parseTrait(s: String): Defn.Trait =
    s.parse[Stat].get.asInstanceOf[Defn.Trait]

  def parseObject(s: String): Defn.Object =
    s.parse[Stat].get.asInstanceOf[Defn.Object]
}
