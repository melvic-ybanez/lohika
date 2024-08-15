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
        Right(Proof(assumptions, prop, "Assumption"))
      )

    // consider property-based testing
    List[Formula]("A", "A" | "B", "A" & "B", "A" ==> "B", "A" <==> "B").foreach(testProp)

  it should "not prove any arbitrary formula that is not assumed and can't be derived" in:
    given assumptions: Assumptions = Assumptions.fromFormulae("B")
    Prover.proveProposition("A") should be(Left(Var("A")))

  "&-introduction rule" should "prove any conjunction if all components are proved" in:
    given assumptions: Assumptions = Assumptions.fromFormulae("A", "B")
    Prover.proveProposition("A" & "B") should be(
      Right(
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

  it should "not derive any formula if at least 1 component is not proved" in:
    given assumptions: Assumptions = Assumptions.fromFormulae("A")
    Prover.proveProposition("A" & "B") should be(Left(Var("B")))
