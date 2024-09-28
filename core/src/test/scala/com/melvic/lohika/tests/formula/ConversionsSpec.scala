package com.melvic.lohika.tests.formula

import com.melvic.lohika.formula.Converter
import com.melvic.lohika.tests.BaseSpec
import com.melvic.lohika.tests.formula.FormulaMappingSupport.{====>, FormulaMapper}

class ConversionsSpec extends BaseSpec:
  "Implication elimination" should "work recursively" in:
    given FormulaMapper = FormulaMapper(Converter.eliminateImplications)

    "!((p | q) & (!q => r) => p => r)" ====> "!(!((p | q) & (!(!q) | r)) | (!p | r))"
    "!(B => A) => C" ====> "!(!(!B | A)) | C"

  it should "work on qualified formulas" in:
    given FormulaMapper = FormulaMapper(Converter.eliminateImplications)

    "A:x,y(P(x) => !Q(y))" ====> "A:x,y(!P(x) | !Q(y))"
    "E:x,y(P(x) => !Q(y))" ====> "E:x,y(!P(x) | !Q(y))"

  "Bi-conditional elimination" should "produce a conjunction of implications" in:
    given FormulaMapper = FormulaMapper(Converter.eliminateBiconditionals)

    "P <=> (Q & R)" ====> "(P => Q & R) & (Q & R => P)"

  it should "work on qualified formulas" in:
    given FormulaMapper = FormulaMapper(Converter.eliminateBiconditionals)

    "A:x,y(P(x) <=> !Q(y))" ====> "A:x,y((P(x) => !Q(y)) & (!Q(y) => P(x)))"
    "E:x,y(P(x) <=> !Q(y))" ====> "E:x,y((P(x) => !Q(y)) & (!Q(y) => P(x)))"

  "Negations" should "be moved inside" in:
    given FormulaMapper = FormulaMapper(Converter.moveNegationsInside)

    "!(!(!B) & !A)" ====> "!B | A"
    "A | !(!(!B) & !A) | C | C" ====> "A | (!B | A) | C | C"
