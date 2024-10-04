package com.melvic.lohika.tests.formula

import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.RenamingPair
import com.melvic.lohika.tests.BaseSpec
import com.melvic.lohika.tests.formula.FormulaMappingSupport.{====>, FormulaMapper}

class AlphaConversionSpec extends BaseSpec:
  given RenamingPair = RenamingPair("x", "y")
  given (using RenamingPair): FormulaMapper = FormulaMapper(Formula.alphaConvert)

  "Alpha-conversion" should "work on simple bound variable" in:
    "A:x(P(x) => Q(x))" ====> "A:y(P(y) => Q(y))"
    "E:xQ(x)"           ====> "E:yQ(y)"

  it should "work even if the quantification is not top-level" in:
    "A:x(P(x) => R(a, x)) & Q(x)" ====> "A:x(P(x) => R(a, x)) & Q(x)"

  it should "not work on nested quantification when the name is shadowed" in:
    "A:x(P(x) => E:x(R(x)))" ====> "A:y(P(y) => E:x(R(x)))"
    "A:x(P(x) => E:z,x(x))"  ====> "A:y(P(y) => E:z,x(x))"

  it should "work on nested quantifier" in:
    "A:x(P(x) => E:z(Q(z) & R(x, z)))" ====> "A:y(P(y) => E:z(Q(z) & R(y, z)))"

  it should "work across multiple quantifiers" in:
    given RenamingPair = RenamingPair("x", "z")
    "E:xA:y(P(x, y) & Q(y))" ====> "E:zA:y(P(z, y) & Q(y))"
