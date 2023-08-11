package org.finos.morphir.internal

import org.finos.morphir.naming._

import scala.annotation.tailrec
trait TypeModule { self: DocumentedModule =>

  sealed trait Type[+A] { self =>
    import Type.*

    def ??(doc: String): Documented[Type[A]] = Documented(doc, this)
    def attributes: A

    final def exists(f: PartialFunction[Type[A], Any]): Boolean = find(f).isDefined

    final def find[Z](f: PartialFunction[Type[A], Z]): Option[Z] = {
      @tailrec
      def loop(typ: Type[A], stack: List[Type[A]]): Option[Z] =
        f.lift(typ) match {
          case Some(z) => Some(z)
          case None =>
            typ match {
              case ExtensibleRecord(_, _, List(head, tail @ _*)) =>
                val next = head.data
                val rest = tail.map(_.data) ++: stack
                loop(next, rest)
              case Function(_, argumentType, resultType) =>
                loop(argumentType, resultType :: stack)
              case Record(_, List(head, tail @ _*)) =>
                val next = head.data
                val rest = tail.map(_.data) ++: stack
                loop(next, rest)
              case Reference(_, _, List(head, tail @ _*)) =>
                loop(head, tail ++: stack)
              case Tuple(_, List(head, tail @ _*)) =>
                loop(head, tail ++: stack)
              case _ =>
                stack match {
                  case head :: tail => loop(head, tail)
                  case Nil          => None
                }
            }
        }

      loop(self, Nil)
    }

    def fieldCount: Int = foldLeftSome[Int](0) {
      case (acc, Record(_, fields))              => acc + fields.size
      case (acc, ExtensibleRecord(_, _, fields)) => acc + fields.size
      case (acc, _)                              => acc
    }

    final def foldLeft[Z](z: Z)(f: (Z, Type[A]) => Z): Z = {
      @tailrec
      def loop(z: Z, typ: Type[A], stack: List[Type[A]]): Z =
        (z, typ) match {
          case (z, ExtensibleRecord(_, _, List(head, tail @ _*))) =>
            val rest = tail.map(_.data).toList
            loop(z, head.data, rest ++ stack)
          case (z, Function(_, argumentType, returnType)) =>
            loop(z, argumentType, returnType :: stack)
          case (z, Record(_, List(head, tail @ _*))) =>
            val rest = tail.map(_.data).toList
            loop(z, head.data, rest ++ stack)
          case (z, Reference(_, _, List(head, tail @ _*))) =>
            loop(z, head, tail.toList ++ stack)
          case (z, Tuple(_, List(head, tail @ _*))) =>
            loop(z, head, tail.toList ++ stack)
          case (z, _) =>
            stack match {
              case head :: tail => loop(z, head, tail)
              case Nil          => z
            }
        }

      loop(z, self, Nil)
    }

    final def foldLeftSome[Z](z: Z)(f: PartialFunction[(Z, Type[A]), Z]): Z = {
      @tailrec
      def loop(z: Z, typ: Type[A], stack: List[Type[A]]): Z =
        (f.applyOrElse[(Z, Type[A]), Z](z -> typ, _ => z), typ) match {
          case (z, ExtensibleRecord(_, _, List(head, tail @ _*))) =>
            val rest = tail.map(_.data).toList
            loop(z, head.data, rest ++ stack)
          case (z, Function(_, argumentType, returnType)) =>
            loop(z, argumentType, returnType :: stack)
          case (z, Record(_, List(head, tail @ _*))) =>
            val rest = tail.map(_.data).toList
            loop(z, head.data, rest ++ stack)
          case (z, Reference(_, _, List(head, tail @ _*))) =>
            loop(z, head, tail.toList ++ stack)
          case (z, Tuple(_, List(head, tail @ _*))) =>
            loop(z, head, tail.toList ++ stack)
          case (z, _) =>
            stack match {
              case head :: tail => loop(z, head, tail)
              case Nil          => z
            }
        }

      loop(z, self, Nil)
    }

    def map[B](f: A => B): Type[B] = ???
  }
  object Type {

    def reference[A](attributes: A)(name: FQName, typeParams: List[Type[A]] = List.empty): Reference[A] =
      Reference(attributes, name, typeParams)

    sealed case class ExtensibleRecord[+A](attributes: A, name: Name, fields: List[Field[A]]) extends Type[A]
    sealed case class Function[+A](attributes: A, argumentType: Type[A], returnType: Type[A]) extends Type[A]
    sealed case class Record[+A](attributes: A, fields: List[Field[A]])                       extends Type[A]
    sealed case class Reference[+A](attributes: A, typeName: FQName, typeParams: List[Type[A]])
        extends Type[A]
    sealed case class Tuple[+A](attributes: A, elements: List[Type[A]]) extends Type[A]
    sealed case class Unit[+A](attributes: A)                           extends Type[A]
    sealed case class Variable[+A](attributes: A, name: Name)           extends Type[A]

  }

  type Field[+A] = FieldT[Type[A]]
  object Field {
    def apply[A](name: String, tpe: Type[A]): Field[A] = FieldT(Name.fromString(name), tpe)
    def apply[A](name: Name, tpe: Type[A]): Field[A]   = FieldT(name, tpe)

  }

  sealed case class FieldT[+T](name: Name, data: T) {
    @inline def tpe: T = data

    def flatMap[T1](f: T => FieldT[T1]): FieldT[T1] = f(data)
    def map[T1](f: T => T1): FieldT[T1]             = copy(data = f(data))
  }
  object FieldT {

    type Untyped = Field[Unit]

    object Untyped {
      def apply(name: Name): FieldT[Unit] = FieldT(name, ())

      def unapply(field: Field[Unit]): Name = field.name
    }
  }
}
