package com.melvic.lohika.core.formula

import com.melvic.lohika.core.formula.Formula
import com.melvic.lohika.core.formula.Formula.{NoIf, NoIff, SimplifiedNegations, Standardized}
import FormulaMappingSupport.{====>, FormulaMapper}
import com.melvic.lohika.core.BaseSpec

class PnfConversionSpec extends BaseSpec with FormulaMappingSupport:
  "PNF conversion" should "keep the formula as-is if quantifiers are already outside" in:
    "A:x(P(x) | Q(x))" ====> "A:x(P(x) | Q(x))"
    "E:x(P(x) | Q(x))" ====> "E:x(P(x) | Q(x))"

  it should "work on nested quantifiers" in:
    given FormulaMapper = FormulaMapper(fm =>
      Formula
        .toPnf(
          Formula.standardize(SimplifiedNegations(Formula.eliminateImplications(NoIff(fm)).raw))
        )
        .raw
    )
    "E:x(P(x) -> A:yQ(y, x))" ====> "E:xA:y(!P(x) | Q(y, x))"

  it should "standardize the formula if necessary" in:
    given FormulaMapper =
      FormulaMapper(fm => Formula.toPnf(Formula.standardize(SimplifiedNegations(NoIf(fm).raw))).raw)
    "A:x(P(x) & E:yQ(y))" ====> "A:xE:y(P(x) & Q(y))"
    "A:x(P(x) & E:xQ(x))" ====> "A:yE:x(P(y) & Q(x))"

  override given formulaMapper: FormulaMapper =
    FormulaMapper(fm => Formula.toPnf(Standardized(fm)).raw)
