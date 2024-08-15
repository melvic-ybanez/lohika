package com.melvic.lohika.tests.proofs

import com.melvic.lohika.Formula.*
import com.melvic.lohika.{Assumptions, Formula, Proof, Prover}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ProofSpec extends AnyFlatSpec with should.Matchers:
  "Any proposition" should "be provable if assumed" in:
    def testProp(prop: Formula): Unit =
      val assumptions = Assumptions.fromFormulae(prop)
      Prover.proveProposition(assumptions, prop) should be(
        Some(Proof(assumptions, prop))
      )

    // consider property-based testing
    List[Formula]("A", "A" | "B", "A" & "B", "A" ==> "B", "A" <==> "B").foreach(testProp)
