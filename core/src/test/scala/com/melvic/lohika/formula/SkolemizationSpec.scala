package com.melvic.lohika.formula

import com.melvic.lohika.BaseSpec
import com.melvic.lohika.formula.Formula.Pnf
import com.melvic.lohika.formula.FormulaMappingSupport.{====>, FormulaMapper}

class SkolemizationSpec extends BaseSpec with FormulaMappingSupport:
  "Skolemization" should "replace existentially quantified variables with Skolem functions" in:
    "A:xE:yP(x, y)" ====> "A:xP(x, f(x))"

  it should "account for all universal quantifiers that come before an existential quantifier" in:
    "A:xA:zE:yP(x, y, z)"         ====> "A:xA:zP(x, f(x, z), z)"
    "A:x,yE:zA:aE:b(P(a) & Q(b))" ====> "A:x,yA:a(P(a) & Q(f(x, y, a)))"
    "A:xE:yA:zE:wP(x, y, z, w)" ====> "A:xA:zP(x, f(x), z, g(x, z))"

  it should "work on nested existential quantifiers" in:
    "A:xE:yE:z(P(x, y) & Q(y, z))" ====> "A:x(P(x, f(x)) & Q(f(x), g(x)))"
    "A:xE:yE:z(P(x, y) & Q(z, y))" ====> "A:x(P(x, f(x)) & Q(g(x), f(x)))"
    "A:xE:yE:z(P(y, z) & Q(x, y))" ====> "A:x(P(f(x), g(x)) & Q(x, f(x)))"
    "A:xE:y,aE:z(P(y, z, a) & Q(x, y))" ====> "A:x(P(f(x), h(x), g(x)) & Q(x, f(x)))"

  override given formulaMapper: FormulaMapper =
    FormulaMapper(fm => Formula.skolemize(Pnf(fm)).raw)
