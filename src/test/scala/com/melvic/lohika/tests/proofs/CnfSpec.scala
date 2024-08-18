package com.melvic.lohika.tests.proofs

import com.melvic.lohika.Cnf
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import com.melvic.lohika.Formula._

class CnfSpec extends AnyFlatSpec with should.Matchers:
  "Disjunction" should "flatten" in:
    Cnf.convert(("A" | "B") | "C") should be(Or.of("A", "B", "C"))
