package com.melvic.lohika.core.formula

import com.melvic.lohika.core.formula.Formula
import com.melvic.lohika.core.formula.Formula.NoIff
import FormulaMappingSupport.{====>, FormulaMapper}
import com.melvic.lohika.core.BaseSpec

class ConversionsSpec extends BaseSpec:
  "Implication elimination" should "work recursively" in:
    given FormulaMapper = FormulaMapper(fm => Formula.eliminateImplications(NoIff(fm)).raw)

    "!((P | Q) & (!Q -> R) -> P -> R)" ====> "!(!((P | Q) & (!(!Q) | R)) | (!P | R))"
    "!(B -> A) -> C"                   ====> "!(!(!B | A)) | C"

  it should "work on qualified formulas" in:
    given FormulaMapper = FormulaMapper(fm => Formula.eliminateImplications(NoIff(fm)).raw)

    "A:x,y(P(x) -> !Q(y))" ====> "A:x,y(!P(x) | !Q(y))"
    "E:x,y(P(x) -> !Q(y))" ====> "E:x,y(!P(x) | !Q(y))"

  "Bi-conditional elimination" should "produce a conjunction of implications" in:
    given FormulaMapper = FormulaMapper(Formula.eliminateBiconditionals(_).raw)

    "P <-> (Q & R)" ====> "(P -> Q & R) & (Q & R -> P)"

  it should "work on qualified formulas" in:
    given FormulaMapper = FormulaMapper(Formula.eliminateBiconditionals(_).raw)

    "A:x,y(P(x) <-> !Q(y))" ====> "A:x,y((P(x) -> !Q(y)) & (!Q(y) -> P(x)))"
    "E:x,y(P(x) <-> !Q(y))" ====> "E:x,y((P(x) -> !Q(y)) & (!Q(y) -> P(x)))"
