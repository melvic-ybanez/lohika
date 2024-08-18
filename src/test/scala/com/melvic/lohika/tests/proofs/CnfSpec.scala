package com.melvic.lohika.tests.proofs

import com.melvic.lohika.Cnf
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import com.melvic.lohika.Formula._

class CnfSpec extends AnyFlatSpec with should.Matchers:
  "Disjunction" should "flatten" in:
    Cnf.convert(("A" | "B") | ("C" | ("D" | "E"))) should be(Or.of("A", "B", "C", "D", "E"))

  "Conjunction" should "flatten" in:
    Cnf.convert(("A" & "B") & ("C" & ("D" & "E"))) should be(And.of("A", "B", "C", "D", "E"))
