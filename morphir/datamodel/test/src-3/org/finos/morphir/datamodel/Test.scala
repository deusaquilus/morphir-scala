package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.Derivers.{given, _}

object Test {
  // Flags.NoInits | Flags.Sealed | Flags.Trait
  sealed trait Faa
  // Flags.Case | Flags.Final | Flags.Module | Flags.NoInits
  case object Baa extends Faa
  // Flags.Final | Flags.Module | Flags.NoInits | Flags.Synthetic
  case class Zaa(value: String) extends Faa

  // Flags.Abstract | Flags.Enum | Flags.NoInits | Flags.Sealed
  enum Foo {
    // Flags.Abstract | Flags.Enum | Flags.NoInits | Flags.Sealed
    case Bar
    // Flags.Case | Flags.Enum | Flags.Final | Flags.NoInits
    case Baz(x: String)
  }

  // DONT use Deriver.gen in same project while working on it
  // if it fails, it messes up IntelliJ's ability to know what
  // is compiled and what is not
  def main(args: Array[String]): Unit = {
    println("hello")
    println("hello") //
    {
      val deriver = Deriver.gen[Faa] //// //
      println(pprint.apply(deriver.concept))
      println(pprint.apply(deriver.derive(Zaa("blah"))))
      println(pprint.apply(deriver.derive(Baa))) //
    }
    {
      val deriver = Deriver.gen[Foo] ////
      println(pprint.apply(deriver.concept))
      println(pprint.apply(Foo.Baz("blah")))
      println(pprint.apply(Foo.Bar))
    }
  }
  // println(Baa.asInstanceOf[Product])

  // println(DeriverMacros.showFlags[Foo.Bar.type]) // // //
}
