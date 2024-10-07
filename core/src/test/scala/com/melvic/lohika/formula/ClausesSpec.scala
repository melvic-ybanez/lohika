package com.melvic.lohika.formula

import com.melvic.lohika.BaseSpec
import com.melvic.lohika.formula.Clauses

class ClausesSpec extends BaseSpec:
  "A formula in CNF" should "be split into multiple clauses" in:
    Clauses.fromFormula("(A | B) & (C | D)") should be(Clauses("A | B", "C | D"))

  "A Problem statement" should "combine all conclusion and premises clauses" in:
    Clauses.fromAllFormulae(
      List("(A | B) & (C | D)", "F & !P", "C", "E | G")
    ) should be(Clauses("A | B", "C | D", "F", "!P", "C", "E | G"))
