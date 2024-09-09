package com.melvic.lohika.tests

import com.melvic.lohika
import com.melvic.lohika.Clauses
import com.melvic.lohika.formula.Formula.*

class ClausesSpec extends BaseSpec:
  "A formula in CNF" should "be split into multiple clauses" in:
    Clauses.fromFormula("(A | B) & (C | D)") should be(lohika.Clauses("A | B", "C | D"))

  "A Problem statement" should "combine all proposition and assumption clauses" in:
    Clauses.fromAllFormulae(
      List("(A | B) & (C | D)", "F & !P", "C", "E | G")
    ) should be(lohika.Clauses("A | B", "C | D", "F", "!P", "C", "E | G"))
