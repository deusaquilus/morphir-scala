package org.finos.morphir.datamodel

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom, summonInline, error, codeOf}
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Label
import org.finos.morphir.datamodel.Concept

trait Deriver[T] {
  def derive(value: T): Data
  def concept: Concept
}

trait SpecificDeriver[T] extends Deriver[T] {
  def derive(value: T): Data
  def concept: Concept
}

private[datamodel] object DeriverTypes {
  type IsProduct[P <: scala.Product] = P
  type IsOption[P <: Option[_]]      = P
}

object Deriver {
  import DeriverTypes._
  import DeriverMacros._

  inline def toData[T](value: T): Data = {
    import org.finos.morphir.datamodel.Derivers.{given, _}
    val deriver = Deriver.gen[T]
    deriver.derive(value)
  }

  inline def showType[T]: String = ${ showTypeImpl[T] }
  def showTypeImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(TypeRepr.of[T].simplified.typeSymbol.name)
  }

  inline def summonSpecificDeriver[T] =
    summonFrom {
      case deriver: SpecificDeriver[T] => deriver
      case _ =>
        error(s"Cannot find specific deriver for type: ${showType[T]}")
    }

  private enum UnionType {
    case SealedTrait
    case Enum
    case Sum
  }

  private inline def deriveSumVariants[Fields <: Tuple, Elems <: Tuple](
      unionType: UnionType
  ): List[SumBuilder.Variant] =
    inline erasedValue[Fields] match {
      case EmptyTuple => Nil

      case _: (field *: fields) =>
        val fieldName = constValue[field].toString
        inline erasedValue[Elems] match {
          case _: (head *: tail) =>
            val ct = summonClassTagOrFail[head].asInstanceOf[ClassTag[Any]]
            val variant =
              unionType match {
                case UnionType.Enum =>
                  // enum case with fields
                  if (isCaseClass[head]) {
                    summonProductDeriver[head] match {
                      case deriver: GenericProductDeriver[Product] =>
                        SumBuilder.EnumProduct(fieldName, ct, deriver)
                      case other =>
                        error("Illegal state, should not be possible")
                    }
                  } // enum case without fields
                  else {
                    SumBuilder.EnumSingleton(fieldName, ct)
                  }
                // for the sum-case just do regular recursive derivation
                case UnionType.Sum =>
                  val deriver = summonDeriver[head].asInstanceOf[Deriver[Any]]
                  SumBuilder.SumVariant(fieldName, ct, deriver)
              }

            // return the variant and recurse
            variant +: deriveSumVariants[fields, tail](unionType)

          case EmptyTuple =>
            error("shuold not be possible")
        }
    }

  inline def deriveProductFields[Fields <: Tuple, Elems <: Tuple](i: Int): List[ProductBuilderField] =
    inline erasedValue[Fields] match {
      case EmptyTuple => Nil

      case _: (field *: fields) =>
        val fieldName = constValue[field].toString
        inline erasedValue[Elems] match {
          case _: (head *: tail) =>
            val derivationStage =
              summonDeriver[head] match {
                case deriver: SpecificDeriver[Any] =>
                  ProductBuilder.Leaf(fieldName, i, deriver)
                case deriver: GenericProductDeriver[Product] =>
                  ProductBuilder.Product(fieldName, i, deriver)
                case deriver: GenericSumDeriver[Any] =>
                  ProductBuilder.Sum(fieldName, i, deriver)
              }
            derivationStage +: deriveProductFields[fields, tail](i + 1)

          case EmptyTuple =>
            error("shuold not be possible")
        }
    }

  // TODO When making a product deriver, make sure to exclude Option[T] since
  //      we want a specific deriver for that, not a generic one.
  inline def gen[T]: Deriver[T] =
    summonFrom {
      // If there is a leaf-level deriver, summon that first. Do NOT EVER try to summon Deriver[T]
      // directly because you will can run into infinite recursive derivation.
      case deriver: SpecificDeriver[T] =>
        deriver
      case ev: Mirror.Of[T] =>
        inline ev match {
          case m: Mirror.ProductOf[IsOption[t]] =>
            error(
              "Cannot summon a generic derivation of Option[T], a specific encoder is required. Have you imported `org.finos.morphir.datamodel.Derviers._` ?"
            )

          case m: Mirror.ProductOf[T] =>
            val stageListTuple = deriveProductFields[m.MirroredElemLabels, m.MirroredElemTypes](0)
            val mirrorProduct  = ProductBuilder.MirrorProduct(stageListTuple)
            GenericProductDeriver
              .make[T & Product](mirrorProduct)
              .asInstanceOf[Deriver[T]] // not sure why the cast is needed

          case m: Mirror.SumOf[T] =>
            val sumTypeName = DeriverMacros.typeName[T]
            val unionType =
              if (isEnum[T]) UnionType.Enum
              else if (isSealedTrait[T]) UnionType.SealedTrait
              else error("Simple union types not allowed yet")

            val variants = deriveSumVariants[m.MirroredElemLabels, m.MirroredElemTypes](unionType)
            val builder =
              unionType match {
                case UnionType.Enum | UnionType.SealedTrait =>
                  SumBuilder(SumBuilder.SumType.Enum(typeName), variants)
              }
            GenericSumDeriver.make[T](builder)
        }
    }
}

trait GenericProductDeriver[T <: Product] extends Deriver[T] {
  def derive(value: T): Data = builder.run(value)
  def builder: ProductBuilder.MirrorProduct
}

trait GenericSumDeriver[T] extends Deriver[T] {
  def derive(value: T): Data =
    builder.run(value)
  def deriveWithTag(value: T)(implicit ct: ClassTag[T]): Data =
    builder.run(value, Some(ct.asInstanceOf[ClassTag[Any]]))
  def builder: SumBuilder
}
object GenericSumDeriver {
  def make[T](sumBuilder: SumBuilder) =
    new GenericSumDeriver[T] {
      val builder = sumBuilder
      val concept = ???
    }
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
