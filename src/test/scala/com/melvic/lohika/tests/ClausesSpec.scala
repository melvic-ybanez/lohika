package com.melvic.lohika.tests

import com.melvic.lohika.{Clauses, Problem}
import com.melvic.lohika.Formula.*

class ClausesSpec extends BaseSpec:
  "A formula in CNF" should "be split into multiple clauses" in:
    Clauses.fromFormula(("A" | "B") & ("C" | "D")) should be(Clauses(List("A" | "B", "C" | "D")))

  "A Problem statement" should "combine all proposition and assumption clauses" in:
    Clauses.fromProblem(
      Problem(
        List(("A" | "B") & ("C" | "D"), "F" & !"P", "C"),
        "E" | "G"
      )
    ) should be(Clauses(List("A" | "B", "C" | "D", "F", !"P", "C", !"E", !"G")))
