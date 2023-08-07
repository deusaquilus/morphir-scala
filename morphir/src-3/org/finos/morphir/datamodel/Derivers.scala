package org.finos.morphir.datamodel

import scala.quoted.*
import scala.deriving.*

implicit inline def autoDeriver[T]: Deriver[T] = Deriver.gen[T]
