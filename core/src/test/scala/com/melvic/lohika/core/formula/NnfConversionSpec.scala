package com.melvic.lohika.core.formula

import com.melvic.lohika.core.BaseSpec
import com.melvic.lohika.core.formula.Formula
import com.melvic.lohika.core.formula.Formula.NoIf
import com.melvic.lohika.core.formula.FormulaMappingSupport.{====>, FormulaMapper}

class NnfConversionSpec extends BaseSpec with FormulaMappingSupport:
  "NNF conversion" should "move negations inside" in:
    "!(A & B)"                  ====> "!A | !B"
    "!(A | B)"                  ====> "!A & !B"
    "!(!(!B) & !A)"             ====> "!B | A"
    "A | !(!(!B) & !A) | C | C" ====> "A | (!B | A) | C | C"

  it should "work on quantified formulas" in:
    "!A:x(P(x))"               ====> "E:x(!P(x))"
    "!E:x(P(x))"               ====> "A:x(!P(x))"
    "!!A:x(P(x) & Q(x))"       ====> "A:x(P(x) & Q(x))"
    "!(A:x(P(x)) & E:y(Q(y)))" ====> "E:x(!P(x)) | A:y(!Q(y))"
    "A:z!(P(z) | Q(z))"        ====> "A:z(!P(z) & !Q(z))"

  override given formulaMapper: FormulaMapper =
    FormulaMapper(fm => Formula.moveNegationsInside(NoIf(fm)).raw)
