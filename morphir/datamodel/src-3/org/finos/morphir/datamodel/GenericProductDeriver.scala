package org.finos.morphir.datamodel

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom, summonInline, error, codeOf}
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Label
import org.finos.morphir.datamodel.Concept

trait GenericProductDeriver[T <: Product] extends Deriver[T] {
  def derive(value: T): Data = builder.run(value)
  def builder: ProductBuilder.MirrorProduct
}

object GenericProductDeriver {
  def make[T <: Product](productBuilder: ProductBuilder.MirrorProduct) =
    new GenericProductDeriver[T] {
      val builder = productBuilder
      val concept: Concept.Record = {
        // Deriver stage contains list of fields and child derivers
        val fields: List[(Label, Concept)] =
          productBuilder.fields.map {
            case ProductBuilder.Leaf(field, _, deriver) =>
              (Label(field), deriver.concept)
            case ProductBuilder.Product(field, _, deriver) =>
              (Label(field), deriver.concept)
            case ProductBuilder.Sum(field, index, deriver) =>
              (Label(field), deriver.concept)
          }
        Concept.Record(fields)
      }
    }

  /**
   * Automatic generator for Product types (and only product types). For anything that is automatically evaluated by the
   * Scala compiler as a implicit (e.g. auto-derivation) we need to be really careful for that not to evaulate for
   * Products and Primitives (and/or Sums) otherwise there is a danger that it will recurse infinately on certain
   * datatypes that are not well-formed. Therefore for products we have a single function that handles derivation only
   * for products and the implicit needed for that (in the Derivers). This is needed for the following purpose.
   *
   * Say that we have a simple case-class hierarchy like this {{ case class Person(name: Name, age: String) case class
   * Name(first: String, last: String)
   *
   * }}
   */
  inline def gen[T <: Product]: GenericProductDeriver[T] =
    summonFrom { case ev: Mirror.Of[T] =>
      inline ev match {
        case m: Mirror.ProductOf[T] =>
          val stageListTuple = Deriver.deriveProductFields[m.MirroredElemLabels, m.MirroredElemTypes](0)
          val mirrorProduct  = ProductBuilder.MirrorProduct(stageListTuple)
          GenericProductDeriver
            .make[T & Product](mirrorProduct)
      }
    }
}
