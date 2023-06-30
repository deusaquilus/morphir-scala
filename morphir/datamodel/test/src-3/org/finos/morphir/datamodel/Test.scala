package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.Derivers.{given, _}

object Test {
  sealed trait Faa
  case object Baa                           extends Faa
  case class Zaa(value: String, blin: Blin) extends Faa

  enum Foo {
    case Bar
    case Baz(x: String, blin: Blin)
  }

  // TODO issues with recursive definitions
  case class Blin(value: Int, values: Map[String, String])

  // DONT use Deriver.gen in same project while working on it
  // if it fails, it messes up IntelliJ's ability to know what
  // is compiled and what is not
  val myMap = Map("foo" -> "bar")

  def main(args: Array[String]): Unit = {
    println("hello")
    println("hello") //
    {
      val deriver = Deriver.gen[Faa] //// //
      println(pprint.apply(deriver.concept, showFieldNames = false, width = 50))
      println(pprint.apply(deriver.derive(Zaa("blah", Blin(123, myMap))), showFieldNames = false, width = 50))
      println(pprint.apply(deriver.derive(Baa), showFieldNames = false, width = 50)) //
    }
    {
      val deriver = Deriver.gen[Foo] ////
      println(pprint.apply(deriver.concept, showFieldNames = false, width = 50))
      println(pprint.apply(Foo.Baz("blah", Blin(123, myMap)), showFieldNames = false, width = 50))
      println(pprint.apply(Foo.Bar, showFieldNames = false, width = 50))
    }
  }
  // println(Baa.asInstanceOf[Product])

  // println(DeriverMacros.showFlags[Foo.Bar.type]) // // //
}
