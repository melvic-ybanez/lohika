package com.melvic.lohika.tests.formula

import com.melvic.lohika.formula.Converter
import com.melvic.lohika.tests.BaseSpec
import com.melvic.lohika.tests.BaseSpec.assertFromInputStrings

class ConversionsSpec extends BaseSpec:
  "All implications" should "be eliminated" in:
    assertFromInputStrings(
      "!((p | q) & (!q => r) => p => r)",
      "!(!((p | q) & (!(!q) | r)) | (!p | r))"
    ): (input, output) =>
      Converter.eliminateImplications(input) should be(output)

    assertFromInputStrings("!(B => A) => C", "!(!(!B | A)) | C"): (input, output) =>
      Converter.eliminateImplications(input) should be(output)

  "Negations" should "be moved inside" in:
    assertFromInputStrings("!(!(!B) & !A)", "!B | A"): (input, output) =>
      Converter.moveNegationsInside(input) should be(output)

    assertFromInputStrings("A | !(!(!B) & !A) | C | C", "A | (!B | A) | C | C"): (input, output) =>
      Converter.moveNegationsInside(input) should be(output)
