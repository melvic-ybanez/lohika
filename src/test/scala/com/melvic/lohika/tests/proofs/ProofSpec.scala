package com.melvic.lohika.tests.proofs

import com.melvic.lohika.Formula.*
import com.melvic.lohika.{Assumption, Derivations, Formula, Proof, Prover}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ProofSpec extends AnyFlatSpec with should.Matchers:
  "Assumption rule" should "prove any assumed proposition" in:
    def testProp(prop: Formula): Unit =
      given derivations: Derivations = Assumption(prop) :: Nil
      Prover.proveProposition(prop) should be(
        Right(Proof.derive(derivations, prop))
      )

    // consider property-based testing
    List[Formula]("A", "A" | "B", "A" & "B", "A" ==> "B", "A" <==> "B").foreach(testProp)

  it should "not prove any arbitrary formula that is not assumed and can't be derived" in:
    given derivations: Derivations = Assumption("B") :: Nil
    Prover.proveProposition("A") should be(Left(Var("A")))

  "&-introduction rule" should "prove any conjunction if all components are proved" in:
    given derivations: Derivations = Assumption("A") :: Assumption("B") :: Nil
    Prover.proveProposition("A" & "B") should be(
      Right(Proof.derive(Proof.assume("A") :: Proof.assume("B") :: Nil, "A" & "B"))
    )

  it should "not derive any formula if at least 1 component is not proved" in:
    given derivations: Derivations = Assumption("A") :: Nil
    Prover.proveProposition("A" & "B") should be(Left(Var("B")))

//  "&-elimination rule" should "produce a proof for each components of a conjunction" in:
//    given derivations: Derivations = Assumption("A" & "B") :: Nil
//    Prover.proveProposition("A") should be(Right(Proof.derive("A")))
//    Prover.proveProposition("B") should be(Right(Proof.derive("B")))
