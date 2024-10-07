package com.melvic.lohika.formula

import com.melvic.lohika.formula.Formula
import FormulaMappingSupport.{====>, FormulaMapper}
import com.melvic.lohika.BaseSpec
import com.melvic.lohika.formula.Formula.NoIf

class StandardizationSpec extends BaseSpec with FormulaMappingSupport:
  "Standardization" should "work on nested quantifiers" in:
    "A:x(P(x) & E:xQ(x))"           ====> "A:y(P(y) & E:xQ(x))"
    "A:x(E:yP(x, y) & E:xQ(x, y))"  ====> "A:z(E:aP(z, a) & E:xQ(x, y))"
    "E:x(P(x) | A:xQ(x))"           ====> "E:y(P(y) | A:xQ(x))"
    "A:x(P(x) & E:xQ(x) & E:yR(y))" ====> "A:z(P(z) & E:xQ(x) & E:yR(y))"

  it should "work with outer connectives" in:
    "A:xP(x) | E:xQ(x)"               ====> "A:yP(y) | E:xQ(x)"
    "E:x(P(x) | A:y(Q(y) & E:xR(x)))" ====> "E:z(P(z) | A:y(Q(y) & E:xR(x)))"

  it should "work with multiple variables" in:
    "A:x(E:yP(x, y) & A:yQ(x, y))" ====> "A:x(E:zP(x, z) & A:yQ(x, y))"

  it should "work with free variables" in:
    "A:xP(x) & E:xQ(x) & R(y)"         ====> "A:zP(z) & E:xQ(x) & R(y)"
    "E:x(P(x) | A:yQ(x, y)) & R(x, z)" ====> "E:a(P(a) | A:yQ(a, y)) & R(x, z)"
    "A:x(P(x) | E:y(Q(x, y) & A:x(R(x) | E:yS(y, x))))" ====> "A:z(P(z) | E:a(Q(z, a) & A:x(R(x) | E:yS(y, x))))"

  override given formulaMapper: FormulaMapper =
    FormulaMapper(fm => Formula.standardize(NoIf(fm)).raw)
