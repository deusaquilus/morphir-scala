package org.finos.morphir.datamodel

// Stub so Scala 2 can compile org.finos.morphir.datamodel package since it requires the Deriver trait
trait Deriver[A] {
  final def apply(value: A) = derive(value)
  def derive(value: A): Data
  def concept: Concept
}
