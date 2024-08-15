package com.melvic.lohika.tests.proofs

import com.melvic.lohika.Formula.Var
import com.melvic.lohika.{Assumptions, Proof, Prover}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ProofSpec extends AnyFlatSpec with should.Matchers:
  "A variable assumption" should "prove the same variable" in:
    val assumptions = Assumptions.fromFormula(Var("A"))
    val proposition = Var("A")
    Prover.proveProposition(assumptions, proposition) should be(
      Some(Proof(assumptions, proposition))
    )
