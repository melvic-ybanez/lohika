package com.melvic.lohika.tests.proofs

import com.melvic.lohika.Cnf
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import com.melvic.lohika.Formula._

class CnfSpec extends AnyFlatSpec with should.Matchers:
  "Disjunction" should "flatten" in:
    Cnf.convertFormula(("A" | "B") | ("C" | ("D" | "E"))) should be(Or.of("A", "B", "C", "D", "E"))

  "Conjunction" should "flatten" in:
    Cnf.convertFormula(("A" & "B") & ("C" & ("D" & "E"))) should be(And.of("A", "B", "C", "D", "E"))

  "p => q" should "become !p | q" in:
    Cnf.convertFormula("p" ==> "q") should be(!"p" | "q")

  "p <=> q" should "become (p => q) & (q => p) and further converted to CNF" in:
    Cnf.convertFormula("p" <==> "q") should be((!"p" | "q") & (!"q" | "p"))
