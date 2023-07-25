package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.namespacing.*

class EnumWrapperSpec extends munit.FunSuite {
  case class MyBool(value: Boolean)
  case class MyByte(value: Byte)
  case class MyDecimal(value: BigDecimal)
  case class MyInteger(value: scala.BigInt)
  case class MyInt16(value: scala.Short)
  case class MyInt32(value: scala.Int)
  case class MyString(value: java.lang.String)
  case class MyLocalDate(value: java.time.LocalDate)
  case class MyMonth(value: java.time.Month)
  case class MyLocalTime(value: java.time.LocalTime)
  case class MyChar(value: scala.Char)
  object MyUnit

  given rootName: GlobalDatamodelContext with {
    override def value = root / "test" % ns / "enumwrapper"
  }

  def enumMaker(label: String, data: Data, concept: Concept) =
    SingleEnumWrapper(label, concept, rootName.value)

  test("Bool Deriver") {
    given SpecificDataEncoder[MyBool] = Data.Boolean.deriveEnumWrapper("MyBoolLabel", _.value)
    val myBoolData                = Data.Boolean(true)
    val maker                     = enumMaker("MyBoolLabel", myBoolData, Concept.Boolean)
    val myBool                    = MyBool(true)
    val myBoolDeriver             = DataEncoder.gen[MyBool]
    assertEquals(myBoolDeriver.concept, maker.concept)
    assertEquals(myBoolDeriver.encode(myBool), maker.construct(myBoolData))
  }

  test("Byte Deriver") {
    given SpecificDataEncoder[MyByte] = Data.Byte.deriveEnumWrapper("MyByteLabel", _.value)
    val myByteData                = Data.Byte(1)
    val maker                     = enumMaker("MyByteLabel", myByteData, Concept.Byte)
    val myByte                    = MyByte(1.toByte)
    val myByteDeriver             = DataEncoder.gen[MyByte]
    assertEquals(myByteDeriver.concept, maker.concept)
    assertEquals(myByteDeriver.encode(myByte), maker.construct(myByteData))
  }

  test("Decimal Deriver") {
    given SpecificDataEncoder[MyDecimal] = Data.Decimal.deriveEnumWrapper("MyDecimalLabel", _.value)
    val myDecimalData                = Data.Decimal(BigDecimal(123))
    val maker                        = enumMaker("MyDecimalLabel", myDecimalData, Concept.Decimal)
    val myDecimal                    = MyDecimal(BigDecimal(123))
    val myDecimalDeriver             = DataEncoder.gen[MyDecimal]
    assertEquals(myDecimalDeriver.concept, maker.concept)
    assertEquals(myDecimalDeriver.encode(myDecimal), maker.construct(myDecimalData))
  }

  test("Integer Deriver") {
    given SpecificDataEncoder[MyInteger] = Data.Integer.deriveEnumWrapper("MyIntegerLabel", _.value)
    val myIntegerData                = Data.Integer(123)
    val maker                        = enumMaker("MyIntegerLabel", myIntegerData, Concept.Integer)
    val myInteger                    = MyInteger(123)
    val myIntegerDeriver             = DataEncoder.gen[MyInteger]
    assertEquals(myIntegerDeriver.concept, maker.concept)
    assertEquals(myIntegerDeriver.encode(myInteger), maker.construct(myIntegerData))
  }

  test("Int16 Deriver") {
    given SpecificDataEncoder[MyInt16] = Data.Int16.deriveEnumWrapper("MyInt16Label", _.value)
    val myInt16Data                = Data.Int16(123)
    val maker                      = enumMaker("MyInt16Label", myInt16Data, Concept.Int16)
    val myInt16                    = MyInt16(123)
    val myInt16Deriver             = DataEncoder.gen[MyInt16]
    assertEquals(myInt16Deriver.concept, maker.concept)
    assertEquals(myInt16Deriver.encode(myInt16), maker.construct(myInt16Data))
  }

  test("Int32 Deriver") {
    given SpecificDataEncoder[MyInt32] = Data.Int32.deriveEnumWrapper("MyInt32Label", _.value)
    val myInt32Data                = Data.Int32(123)
    val maker                      = enumMaker("MyInt32Label", myInt32Data, Concept.Int32)
    val myInt32                    = MyInt32(123)
    val myInt32Deriver             = DataEncoder.gen[MyInt32]
    assertEquals(myInt32Deriver.concept, maker.concept)
    assertEquals(myInt32Deriver.encode(myInt32), maker.construct(myInt32Data))
  }

  test("String Deriver") {
    given SpecificDataEncoder[MyString] = Data.String.deriveEnumWrapper("MyStringLabel", _.value)
    val myStringData                = Data.String("123")
    val maker                       = enumMaker("MyStringLabel", myStringData, Concept.String)
    val myString                    = MyString("123")
    val myStringDeriver             = DataEncoder.gen[MyString]
    assertEquals(myStringDeriver.concept, maker.concept)
    assertEquals(myStringDeriver.encode(myString), maker.construct(myStringData))
  }

  test("LocalDate Deriver") {
    given SpecificDataEncoder[MyLocalDate] = Data.LocalDate.deriveEnumWrapper("MyLocalDateLabel", _.value)
    val myLocalDateData                = Data.LocalDate(java.time.LocalDate.now)
    val maker                          = enumMaker("MyLocalDateLabel", myLocalDateData, Concept.LocalDate)
    val myLocalDate                    = MyLocalDate(java.time.LocalDate.now)
    val myLocalDateDeriver             = DataEncoder.gen[MyLocalDate]
    assertEquals(myLocalDateDeriver.concept, maker.concept)
    assertEquals(myLocalDateDeriver.encode(myLocalDate), maker.construct(myLocalDateData))
  }

  test("Char Deriver") {
    given SpecificDataEncoder[MyChar] = Data.Char.deriveEnumWrapper("MyCharLabel", _.value)
    val myCharData                = Data.Char('a')
    val maker                     = enumMaker("MyCharLabel", myCharData, Concept.Char)
    val myChar                    = MyChar('a')
    val myCharDeriver             = DataEncoder.gen[MyChar]
    assertEquals(myCharDeriver.concept, maker.concept)
    assertEquals(myCharDeriver.encode(myChar), maker.construct(myCharData))
  }

  test("Unit Deriver") {
    given SpecificDataEncoder[MyUnit.type] = Data.Unit.deriveEnumWrapper("MyUnitLabel")
    val maker                          = UnitEnumWrapper("MyUnitLabel", rootName.value)
    val myUnit                         = MyUnit
    val myUnitDeriver                  = DataEncoder.gen[MyUnit.type]
    assertEquals(myUnitDeriver.concept, maker.concept)
    assertEquals(myUnitDeriver.encode(myUnit), maker.construct)
  }
}
