package com.melvic.lohika.tests.formula

import com.melvic.lohika.formula.AlphaConverter
import com.melvic.lohika.formula.AlphaConverter.RenamingPair
import com.melvic.lohika.tests.BaseSpec
import com.melvic.lohika.tests.formula.FormulaMappingSupport.{====>, FormulaMapper}

class AlphaConverterSpec extends BaseSpec:
  given RenamingPair = RenamingPair("x", "y")
  given (using RenamingPair): FormulaMapper = FormulaMapper(AlphaConverter.convertFormula)

  "Alpha-conversion" should "work on simple bound variable" in:
    "A:x(P(x) => Q(x))" ====> "A:y(P(y) => Q(y))"

  it should "work even if the quantification is not top-level" in:
    "A:x(P(x) => R(a, x)) & Q(x)" ====> "A:x(P(x) => R(a, x)) & Q(x)"

  it should "not work on nested quantification when the name is shadowed" in:
    "A:x(P(x) => E:x(R(x)))" ====> "A:y(P(y) => E:x(R(x)))"
    "A:x(P(x) => E:z,x(x))" ====> "A:y(P(y) => E:z,x(x))"

  it should "work on nested quantifier" in:
    "A:x(P(x) => E:z(Q(z) & R(x, z)))" ====> "A:y(P(y) => E:z(Q(z) & R(y, z)))"

  it should "work across multiple quantifiers" in:
    given RenamingPair = RenamingPair("x", "z")
    "E:x(A:y(P(x, y) & Q(y)))" ====> "E:z(A:y(P(z, y) & Q(y)))"
