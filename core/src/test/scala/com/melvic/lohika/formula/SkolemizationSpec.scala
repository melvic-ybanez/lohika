package com.melvic.lohika.formula

import com.melvic.lohika.BaseSpec
import com.melvic.lohika.formula.Formula.Pnf
import com.melvic.lohika.formula.FormulaMappingSupport.{====>, FormulaMapper}

class SkolemizationSpec extends BaseSpec with FormulaMappingSupport:
  "Skolemization" should "replace existentially quantified variables with Skolem functions" in:
    "A:xE:yP(x, y)" ====> "A:xP(x, f(x))"

  it should "account for all universal quantifiers that come " +
      "before an existential quantifier" in:
    "A:xA:zE:yP(x, y, z)" ====> "A:xA:zP(x, f(x, z), z)"
    "A:x,yE:zA:aE:b(P(a) & Q(b))" ====> "A:x,yA:a(P(a) & Q(f(x, y, a)))"

  override given formulaMapper: FormulaMapper =
    FormulaMapper(fm => Formula.skolemize(Pnf(fm)).raw)
