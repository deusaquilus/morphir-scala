package org.finos.morphir.datamodel

import scala.quoted.*
import scala.reflect.ClassTag

object DeriverMacros {
  import DeriverTypes._

  inline def typeName[T]: String = ${ typeNameImpl[T] }
  def typeNameImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(TypeRepr.of[T].typeSymbol.name)
  }

  inline def showFlags[T]: String = ${ showFlagsImpl[T] }
  def showFlagsImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(TypeRepr.of[T].typeSymbol.flags.show)
  }

  private def flagsOf[T: Type](using Quotes): quotes.reflect.Flags = {
    import quotes.reflect._
    TypeRepr.of[T].typeSymbol.flags
  }

  inline def isEnum[T]: Boolean = ${ isEnumImpl[T] }
  def isEnumImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect._
    Expr(flagsOf[T].is(Flags.Sealed & Flags.Trait))
  }

  inline def isSealedTrait[T]: Boolean = ${ isSealedTraitImpl[T] }
  def isSealedTraitImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect._
    Expr(flagsOf[T].is(Flags.Enum))
  }

  inline def isCaseClass[T]: Boolean = ${ isCaseClassImpl[T] }
  def isCaseClassImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect._
    Expr(flagsOf[T].is(Flags.Case))
  }

  inline def summonClassTagOrFail[T]: ClassTag[T] = ${ summonClassTagOrFailImpl[T] }
  def summonClassTagOrFailImpl[T: Type](using Quotes): Expr[ClassTag[T]] = {
    import quotes.reflect._
    Expr.summon[ClassTag[T]] match {
      case Some(value) => value
      case None =>
        report.errorAndAbort(s"A classTag for the type ${TypeRepr.of[T].show} could not be found!")
    }
  }

  inline def summonDeriver[T]: Deriver[T] = ${ summonDeriverImpl[T] }
  def summonDeriverImpl[T: Type](using Quotes): Expr[Deriver[T]] =
    import quotes.reflect._
    // TODO Summoning generic sum deriver
    val specificDriver = Expr.summon[SpecificDeriver[T]]
    specificDriver match {
      case Some(value) => value
      case None =>
        Type.of[T] match {
          case '[IsProduct[p]] =>
            val genericDeriver = Expr.summon[GenericProductDeriver[p]]
            genericDeriver match {
              case Some(value) => '{ $value.asInstanceOf[Deriver[T]] }
              case _ =>
                report.errorAndAbort(
                  s"Cannot summon specific or generic Deriver for the type: ${TypeRepr.of[T].widen.show}"
                )
            }
          case _ =>
            report.errorAndAbort(
              s"Cannot summon specific Deriver for the type (and it is not a Product): ${TypeRepr.of[T].widen.show}"
            )
        }

    }

  inline def summonProductDeriver[T]: Deriver[T] = ${ summonProductDeriverImpl[T] }
  def summonProductDeriverImpl[T: Type](using Quotes): Expr[Deriver[T]] =
    import quotes.reflect._
    Type.of[T] match {
      case '[IsProduct[p]] =>
        val genericDeriver = Expr.summon[GenericProductDeriver[p]]
        genericDeriver match {
          case Some(value) => '{ $value.asInstanceOf[Deriver[T]] }
          case _ =>
            report.errorAndAbort(
              s"Cannot summon generic Deriver Product for the Product type: ${TypeRepr.of[T].widen.show}"
            )
        }
      case _ =>
        report.errorAndAbort(
          s"Cannot summon generic Deriver for the type (was not a Product): ${TypeRepr.of[T].widen.show}"
        )
    }
}
