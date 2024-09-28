package com.melvic.lohika.tests.formula

import cats.*
import cats.implicits.*
import com.melvic.lohika.Formatter
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.tests.BaseSpec.assertFromInputStrings
import com.melvic.lohika.tests.formula.FormulaMappingSupport.FormulaMapper

import scala.annotation.targetName

trait FormulaMappingSupport:
  given formulaMapper: FormulaMapper

object FormulaMappingSupport:
  final case class FormulaMapper(map: Endo[Formula])

  extension (self: String)
    @targetName("assertTransformedFormula")
    def ====>(other: String)(using formulaMap: FormulaMapper, formatter: Formatter): Unit =
      assertFromInputStrings(self, other): (formula, expectedTransformed) =>
        val transformed = formulaMap.map(formula)
        assert(
          transformed == expectedTransformed,
          show"\n$formula has the expected transformed form: $expectedTransformed. Got: $transformed"
        )
