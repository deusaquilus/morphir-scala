package org.finos
package morphir
package testing

import zio.Chunk
import org.finos.morphir.Dsl
import org.finos.morphir.IR.TypeConstructorInfo
import org.finos.morphir.ir.Type.{Type, UType, reference => typeRef, unitType}
import org.finos.morphir.ir.Value.{Definition => ValueDefinition, RawValue, _}
import org.finos.morphir.ir.sdk.Basics.{add, intType, subtract}
import org.finos.morphir.ir.sdk.{String => StringModule}
import org.finos.morphir.naming._
import org.finos.morphir.syntax.AllSyntax

object CaseExample extends AllSyntax {
  private val intValueDef = valueDef(intType)

  // /x = if (foo) y else 0
  // y = if (!foo) x else 0
  val letIntroduceMultipleExample: RawValue = letRec(
    Map(
      Name.fromString("x") -> ValueDefinition.fromRawValue(int(20), intType), // lit(20)
      Name.fromString("y") -> ValueDefinition.fromRawValue(int(22), intType)  // lit(22)
    ),
    apply(apply(add, variable("x")), variable("y"))
    // nativeApply(
    //   NativeFunction.Addition,
    //   Chunk(variable("x"), variable("y"))
    // )
  )

  val letIntroduceOutOfOrderExample: RawValue = letRec(
    Map(
      Name.fromString("x") -> intValueDef(apply(apply(add, variable("y")), int(22))),
      // nativeApply(
      //   NativeFunction.Addition,
      //   Chunk(
      //     variable("y"),
      //     int(22)
      //   )
      // ).toDefinition(Basics.intType),
      Name.fromString("y") -> intValueDef(int(22))
    ),
    inValue = variable("x")
  )

  val applyFieldFunction: RawValue =
    apply(fieldFunction(Name.fromString("fieldA")), recordCaseExample)

  val additionExample: RawValue =
    let(
      "x",
      intValueDef(int(1)),
      let(
        "y",
        intValueDef(int(2)),
        apply(apply(add, variable("x")), variable("y"))
        // nativeApply(
        //   NativeFunction.Addition,
        //   Chunk(variable("x"), variable("y"))
        // )
      )
    )

  val subtractionExample: RawValue =
    let(
      "x",
      intValueDef(int(1)),
      let(
        Name("y"),
        intValueDef(int(2)),
        apply(apply(subtract, variable("x")), variable("y"))
        // nativeApply(
        //   NativeFunction.Subtraction,
        //   Chunk(variable(Name("x")), variable(Name("y")))
        // )
      )
    )

  val tupleCaseExample: RawValue = tuple(int(1), int(2))

  val listCaseExample: RawValue =
    list(string("hello"), string("world"))

  val ifThenElseCaseExample: RawValue =
    ifThenElse(
      condition = boolean(false),
      thenBranch = string("yes"),
      elseBranch = string("no")
    )

  lazy val recordCaseExample: RawValue = {
    val fieldA = Name.fromString("fieldA")
    val fieldB = Name.fromString("fieldB")

    val value1 = string("hello")
    val value2 = int(2)

    recordRaw { rb =>
      import rb._
      addField(fieldA, value1)
      addField(fieldB, value2)
    }
  }

  val recordCaseUpdateExample: RawValue =
    update(
      recordCaseExample,
      Chunk(
        Name("fieldB") -> int(3)
      )
    )

  val patternMatchWildcardCaseExample: RawValue =
    patternMatch(
      wholeNumber(42),
      wildcardPattern -> wholeNumber(100)
    )

  val patternMatchAsCaseExample: RawValue =
    patternMatch(
      wholeNumber(42),
      asPattern(wildcardPattern, Name.fromString("x")) -> variable(Name.fromString("x"))
    )

  val patternMatchEmptyListCaseExample: RawValue =
    patternMatch(
      list(),
      emptyListPattern -> string("empty list")
    )

  val patternHeadTailCaseExample: RawValue =
    patternMatch(
      listCaseExample,
      headTailPattern(
        stringPattern("hello"),
        asPattern(wildcardPattern, Name("tail"))
      ) -> variable("tail")
    )

  val patternTupleOneCaseExample: RawValue =
    patternMatch(
      tuple(string("singleton tuple")),
      tuplePattern(asPattern(wildcardPattern, Name("x"))) -> variable(Name("x"))
    )

  val patternTupleOneCaseCounterExample: RawValue =
    patternMatch(
      string("singleton tuple"),
      tuplePattern(wildcardPattern) -> string("wrong"),
      wildcardPattern               -> string("right")
    )

  val patternTupleCaseExample: RawValue =
    patternMatch(
      tupleCaseExample,
      tuplePattern(
        wildcardPattern,
        wildcardPattern
      ) -> wholeNumber(107)
    )

  lazy val patternConstructorCaseExample: RawValue =
    patternMatch(
      checkingAccountConstructorExample,
      constructorPattern(
        checkingAccountTypeName,
        Chunk(wildcardPattern, asPattern(wildcardPattern, Name("x")))
      ) -> variable(Name("x"))
    )

  val patternUnitCaseExample: RawValue =
    patternMatch(
      unit,
      emptyListPattern -> string("wrong"),
      unitPattern      -> string("right")
    )

  val letDestructExample: RawValue =
    destructure(
      tuplePattern(asPattern(wildcardPattern, Name("x")), asPattern(wildcardPattern, Name("y"))),
      tuple(string("red"), string("blue")),
      variable("x")
    )
  val staticScopingExample: RawValue =
    letDef(
      Name("x"),
      valueDef(StringModule.stringType)(string("static")),
      letRec(
        Map(Name("y") -> valueDef(StringModule.stringType)(variable("x"))),
        let(Name("x"), valueDef(StringModule.stringType)(string("dynamic")), variable(Name("y")))
      )
    )
  val letRecExample: RawValue =
    letRec(
      Map(
        Name.fromString("x") -> intValueDef(
          ifThenElse(
            condition = boolean(false),
            thenBranch = variable("y"),
            elseBranch = int(3)
          )
        ),
        Name.fromString("y") ->
          intValueDef(
            ifThenElse(
              condition = boolean(false),
              thenBranch = int(2),
              elseBranch = variable("x")
            )
          )
      ),
      apply(apply(add, variable("x")), variable("y"))
      // nativeApply(
      //   NativeFunction.Addition,
      //   Chunk(
      //     variable("x"),
      //     variable("y")
      //   )
      // )
    )

  // // (valueDefinitions: Map[Name, Self], inValue: Self)

  // // example : letrec (x, y) = if (cond) then (0, x) else (y, 0)

  /**
   * letRec x -> 3 y -> 4 x + y
   */

  val patternMatchAsCaseComplexExample: RawValue =
    patternMatch(
      wholeNumber(7),
      asPattern(
        intPattern(8),
        Name.fromString("x")
      ) -> apply(apply(subtract, variable("x")), variable("y")),
      // nativeApply(
      //   NativeFunction.Subtraction,
      //   Chunk(
      //     variable(Name.fromString("x")),
      //     variable(Name.fromString("x"))
      //   )
      // ),
      asPattern(intPattern(7), Name.fromString("x")) ->
        apply(apply(add, variable("x")), variable("x"))
      // nativeApply(
      //   NativeFunction.Addition,
      //   Chunk(
      //     variable(Name.fromString("x")),
      //     variable(Name.fromString("x"))
      //   )
      // )
    )

  // // { case _ => 42}()

  val applyWithWildCard: RawValue =
    apply(lambda(wildcardPattern, wholeNumber(42)), unit)

  val lambdaExample: RawValue = let(
    Name("foo"),
    intValueDef(
      lambda(
        asPattern(wildcardPattern, Name("x")),
        apply(apply(add, variable("x")), variable("x"))
        // nativeApply(
        //   NativeFunction.Addition,
        //   Chunk(
        //     variable("x"),
        //     variable("x")
        //   )
        // )
      )
    ),
    apply(variable("foo"), int(33))
  )

  val personName: FQName =
    FQName(PackageName(Path(Name(""))), ModuleName(Path(Name(""))), Name("Person"))
  lazy val recordTypeName: FQName =
    FQName(
      PackageName(Path(Name("Morphir.SDK"))),
      ModuleName(Path(Name("Morphir.SDK"))),
      Name("RecordType")
    )

  lazy val recordType: UType = ir.Type.record(
    ir.Type.field(Name("name"), unitType),
    ir.Type.field(Name("age"), unitType)
  )

  lazy val recordTypeAliasSpecification: morphir.ir.Type.Specification.TypeAliasSpecification[scala.Unit] =
    morphir.ir.Type.Specification.TypeAliasSpecification[scala.Unit](
      typeParams = Chunk.empty,
      expr = recordType
    )

  lazy val accountTypeName: FQName = FQName(
    PackageName(Path(Name("Morphir.SDK"))),
    ModuleName(Path(Name("Morphir.SDK.Account"))),
    Name("Account")
  )

  lazy val savingsAccountTypeName: FQName = FQName(
    PackageName(Path(Name("Morphir.SDK"))),
    ModuleName(Path(Name("Morphir.SDK.Account"))),
    Name("SavingsAccount")
  )
  lazy val checkingAccountTypeName: FQName = FQName(
    PackageName(Path(Name("Morphir.SDK"))),
    ModuleName(Path(Name("Morphir.SDK.Account"))),
    Name("CheckingAccount")
  )

  lazy val savingsAccountTypeConstructor: TypeConstructorInfo = TypeConstructorInfo(
    containingType = accountTypeName,
    typeParams = Chunk.empty,
    typeArgs = Chunk(Name.fromString("arg1") -> typeRef(FQName.fromString("Morphir.SDK.String"), Chunk.empty))
  )

  lazy val checkingAccountTypeConstructor: TypeConstructorInfo = TypeConstructorInfo(
    containingType = accountTypeName,
    typeParams = Chunk.empty,
    typeArgs = Chunk(
      Name.fromString("arg1") -> typeRef(FQName.fromString(":Morphir.SDK:String"), Chunk.empty),
      Name.fromString("arg2") -> typeRef(FQName.fromString(":Morphir.SDK:Int"), Chunk.empty)
    )
  )

  val constructorExample: RawValue =
    apply(
      constructor(recordTypeName),
      string("Adam").toRawValue,
      int(42)
    )

  val savingsAccountConstructorExample: RawValue =
    apply(
      constructor(savingsAccountTypeName),
      string("Adam")
    )

  val checkingAccountConstructorExample: RawValue =
    apply(
      (),
      constructor(checkingAccountTypeName),
      string("Brad"),
      int(10000)
    )

  // tuple ("Adam", 42)
  // record (name: "Adam", age: 42)
  // extensiblerecord (Person((name: "Adam", age: 42)

}
