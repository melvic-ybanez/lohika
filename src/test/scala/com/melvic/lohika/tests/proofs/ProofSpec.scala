package com.melvic.lohika.tests.proofs

import com.melvic.lohika.Formula.*
import com.melvic.lohika.{Assumptions, Formula, Proof, Prover}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ProofSpec extends AnyFlatSpec with should.Matchers:
  "Assumption rule" should "prove any assumed proposition" in:
    def testProp(prop: Formula): Unit =
      given assumptions: Assumptions = Assumptions.fromFormulae(prop)
      Prover.proveProposition(prop) should be(
        Some(Proof(assumptions, prop, "Assumption"))
      )

    // consider property-based testing
    List[Formula]("A", "A" | "B", "A" & "B", "A" ==> "B", "A" <==> "B").foreach(testProp)

  "&-introduction rule" should "prove any conjunction if all components are proved" in:
    given assumptions: Assumptions = Assumptions.fromFormulae("A", "B")
    Prover.proveProposition("A" & "B") should be(
      Some(
        Proof(
          Assumptions.fromProofs(
            Proof.assume("A"),
            Proof.assume("B")
          ),
          "A" & "B",
          "&-introduction"
        )
      )
    )
