package org.finos.morphir.datamodel

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
    case Baz(x: Int)
  }

  def main(args: Array[String]): Unit =
    println(DeriverMacros.showFlags[Foo.Bar.type]) // //
}
