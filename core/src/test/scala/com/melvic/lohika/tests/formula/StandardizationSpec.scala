package com.melvic.lohika.tests.formula

import com.melvic.lohika.formula.{Formula, Standardization}
import com.melvic.lohika.tests.BaseSpec
import com.melvic.lohika.tests.formula.FormulaMappingSupport.{====>, FormulaMapper}

class StandardizationSpec extends BaseSpec with FormulaMappingSupport:
  "Standardization" should "work on nested quantifiers" in:
    "A:x(P(x) & E:xQ(x))"          ====> "A:x(P(x) & E:yQ(y))"
    "A:x(E:yP(x, y) & E:xQ(x, y))" ====> "A:x(E:yP(x, y) & E:zQ(z, y))"
    "E:x(P(x) | A:xQ(x))"          ====> "E:x(P(x) | A:yQ(y))"

  it should "work with outer connectives" in:
    "A:xP(x) | E:xQ(x)"               ====> "A:xP(x) | E:yQ(y)"
    "E:x(P(x) | A:y(Q(y) & E:xR(x)))" ====> "E:x(P(x) | A:y(Q(y) & E:zR(z)))"

  it should "work with multiple variables" in:
    "A:x(E:yP(x, y) & A:yQ(x, y))" ====> "A:x(E:yP(x, y) & A:zQ(x, z))"

//  it should "work with free variables" in:
//    "A:xP(x) & E:xQ(x) & R(y)" ====> "A:xP(x) & E:zQ(z) & R(y)"

  override given formulaMapper: FormulaMapper =
    FormulaMapper(Formula.standardize(_).run(Nil).value._2)
