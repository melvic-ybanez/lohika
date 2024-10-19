package com.melvic.lohika.formula

import com.melvic.lohika.BaseSpec
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.formula.FormulaMappingSupport.{====>, FormulaMapper}

class SkolemizationSpec extends BaseSpec with FormulaMappingSupport:
  "Skolemization" should "replace existentially quantified variables with Skolem functions" in:
    "A:xE:yP(x, y)"       ====> "A:xP(x, f_1(x))"
    "A:xE:y(P(x) | Q(y))" ====> "A:x(P(x) | Q(f_1(x)))"

  it should "replace variables with Skolem constants when there's no preceding universal quantifier" in:
    "E:xP(x)"       ====> "P('x_1)"
    "E:xA:yP(x, y)" ====> "A:yP('x_1, y)"

  it should "account for all universal quantifiers that come before an existential quantifier" in:
    "A:xA:zE:yP(x, y, z)"         ====> "A:xA:zP(x, f_1(x, z), z)"
    "A:x,yE:zA:aE:b(P(a) & Q(b))" ====> "A:x,yA:a(P(a) & Q(f_1(x, y, a)))"
    "A:xE:yA:zE:wP(x, y, z, w)"   ====> "A:xA:zP(x, f_1(x), z, g_1(x, z))"
    "A:xA:yE:zE:w(P(x, y, z) & Q(z, w))" ====> "A:xA:y(P(x, y, f_1(x, y)) & Q(f_1(x, y), g_1(x, y)))"
    "E:xA:yE:zE:w(R(x, y) | (S(z, w) & T(y)))" ====> "A:y(R('x_1, y) | S(f_1(y), g_1(y)) & T(y))"

  it should "work on nested existential quantifiers" in:
    "A:xE:yE:z(P(x, y) & Q(y, z))"      ====> "A:x(P(x, f_1(x)) & Q(f_1(x), g_1(x)))"
    "A:xE:yE:z(P(x, y) & Q(z, y))"      ====> "A:x(P(x, f_1(x)) & Q(g_1(x), f_1(x)))"
    "A:xE:yE:z(P(y, z) & Q(x, y))"      ====> "A:x(P(f_1(x), g_1(x)) & Q(x, f_1(x)))"
    "A:xE:y,aE:z(P(y, z, a) & Q(x, y))" ====> "A:x(P(f_1(x), h_1(x), g_1(x)) & Q(x, f_1(x)))"

  override given formulaMapper: FormulaMapper =
    FormulaMapper(fm => Formula.skolemize(Pnf(fm)).raw)
