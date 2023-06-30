package org.finos.morphir.datamodel

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{codeOf, constValue, erasedValue, error, summonFrom, summonInline}
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Label

private[datamodel] sealed trait ProductBuilder

private[datamodel] sealed trait ProductBuilderField extends ProductBuilder {
  def run(parent: scala.Product): Data
  def field: String
}
private[datamodel] object ProductBuilder {
  case class Leaf(field: String, index: Int, deriver: SpecificDeriver[Any]) extends ProductBuilderField {
    def run(parent: scala.Product) = deriver.derive(parent.productElement(index))
  }

  private def fail(derived: Any, index: Int) =
    throw new IllegalArgumentException(
      s"The derived output element ${derived} at index: $index was not a product-type, ${derived.getClass()}"
    )

  case class Product(field: String, index: Int, deriver: GenericProductDeriver[scala.Product])
      extends ProductBuilderField {
    def run(parent: scala.Product) =
      deriver.derive {
        val derived = parent.productElement(index)
        derived match {
          case p: scala.Product => p
          case _                => fail(derived, index)
        }
      }
  }
  case class Sum(field: String, index: Int, deriver: GenericSumDeriver[Any]) extends ProductBuilderField {
    def run(parent: scala.Product) =
      deriver.derive {
        val derived = parent.productElement(index)
        derived match {
          case p: scala.Product => p
          case _                => fail(derived, index)
        }
      }
  }

  case class MirrorProduct(fields: List[ProductBuilderField]) extends ProductBuilder {
    def run(parent: scala.Product) =
      Data.Record(fields.map(f => (Label(f.field), f.run(parent))))
  }
}

private[datamodel] case class SumBuilder(tpe: SumBuilder.SumType, variants: List[SumBuilder.Variant]) {
  private def failInsideNotProduct(derivedAs: Any) =
    throw new IllegalArgumentException(
      s"Inner enum data (for: ${tpe}) is only allowed to come from a Scala product but it was derived as: $derivedAs"
    )

  def run(value: Any, tag: Option[ClassTag[Any]] = None): Data = {
    val usedVariant =
      tag match {
        case Some(tag) =>
          variants.find(v => tag <:< v.tag) match {
            case Some(value) => value
            case None =>
              val varNames = variants.map(v => s"${v.enumLabel}:${v.tag.runtimeClass.getName}")
              throw new IllegalArgumentException(
                s"Cannot decode instance of ${tag.runtimeClass.getName} (value was ${value}:${value.getClass.getSimpleName}) becase it was not any of the possibilities: ${varNames}"
              )
          }
        // If we don't have a class-tag need to rely on JVM-level reflection (not sure this is implemented in ScalaJS)
        case None =>
          val cls = value.getClass
          variants.find(v => cls.isAssignableFrom(v.tag.runtimeClass)) match {
            case Some(value) => value
            case None =>
              val varNames = variants.map(v => s"${v.enumLabel}:${v.tag.runtimeClass.getName}")
              throw new IllegalArgumentException(
                s"Cannot decode instance of ${value}:${cls.getName} because it was not a sub-type of any of the possibilities: ${varNames}"
              )
          }
      }

    val enumValues =
      usedVariant match {
        // for a enum case object, data-type is just a 'unit'
        case SumBuilder.EnumSingleton(enumLabel, tag) =>
          List(Data.Unit)

        case v: SumBuilder.EnumProduct =>
          value match {
            case p: Product =>
              val enumCaseRecord = v.deriver.derive(p)
              enumCaseRecord match {
                case Data.Record(values) =>
                  values.map { (_, data) => data }
                case other =>
                  failInsideNotProduct(other)
              }
            case other => throw new IllegalArgumentException(
                s"The value ($value) for the enum variant ${v.enumLabel} must be a scala product type (case class or multi-field enum) but it was a ${other.getClass}"
              )
          }
      }

    // TODO what if it's a singleton e.g. a case-object, maybe need another SumBuilder type for that
    val (enumType, enumLabel) =
      tpe match {
        case SumBuilder.SumType.Enum(name) =>
          val enumCases =
            variants.map { v =>
              val enumVariantFields =
                v match {
                  case SumBuilder.EnumSingleton(enumLabel, tag) => ???
                  case pvar: SumBuilder.EnumProduct =>
                    pvar.deriver.concept match {
                      case Concept.Record(fields) =>
                        fields.map { case (label, concept) => Concept.Enum.Case.Field.Named(label, concept) }
                      case other =>
                        failInsideNotProduct(other)
                    }
                }
              Concept.Enum.Case(Label(v.enumLabel), enumVariantFields)
            }
          (Concept.Enum(name, enumCases), name)
      }

    Data.Case(enumValues, enumLabel, enumType)
  }
}
object SumBuilder {
  sealed trait Variant {
    def tag: ClassTag[Any]
    def enumLabel: java.lang.String
  }
  sealed trait EnumVariant extends Variant
  // case object variant of a sealed trait or a enum case with no fields
  case class EnumSingleton(enumLabel: java.lang.String, tag: ClassTag[Any])
      extends EnumVariant
  // case class variant of sealed trait or enum case with fields
  case class EnumProduct(enumLabel: java.lang.String, tag: ClassTag[Any], deriver: GenericProductDeriver[Product])
      extends EnumVariant

  // for generic sums
  case class SumVariant(enumLabel: java.lang.String, tag: ClassTag[Any], deriver: Deriver[Any]) extends Variant

  sealed trait SumType
  object SumType {
    case class Enum(name: java.lang.String) extends SumType
    // TODO Union for non-discrimited unions
  }
}
