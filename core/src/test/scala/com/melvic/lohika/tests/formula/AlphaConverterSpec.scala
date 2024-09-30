package com.melvic.lohika.tests.formula

import com.melvic.lohika.formula.AlphaConverter
import com.melvic.lohika.formula.AlphaConverter.NameConversionTable
import com.melvic.lohika.tests.BaseSpec
import com.melvic.lohika.tests.formula.FormulaMappingSupport.{====>, FormulaMapper}

class AlphaConverterSpec extends BaseSpec:
  given (using NameConversionTable): FormulaMapper = FormulaMapper(AlphaConverter.convertFormula)

  "Alpha-conversion" should "work on simple bound variable" in:
    given NameConversionTable = Map("x" -> "y")

    "A:x(P(x) => Q(x))" ====> "A:y(P(y) => Q(y))"

  it should "work even if the quantification is not top-level" in:
    given NameConversionTable = Map("x" -> "y")
    "A:x(P(x) => R(a, x)) & Q(x)" ====> "A:y(P(y) => R(a, y)) & Q(x)"

