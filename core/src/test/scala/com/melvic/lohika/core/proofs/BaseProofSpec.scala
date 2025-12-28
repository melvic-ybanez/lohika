package com.melvic.lohika.core.proofs

import com.melvic.lohika.core.formula.Cnf.Clause
import com.melvic.lohika.core.proofs.BaseProofSpec.result
import com.melvic.lohika.core.prover.Proof.Step
import com.melvic.lohika.core.prover.Proof.Step.{Contradiction, Exhaustion}
import com.melvic.lohika.core.prover.{Proof, Prover}
import com.melvic.lohika.core.prover.Prover.given
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BaseProofSpec extends AnyFlatSpec with should.Matchers:
  def contradiction(entailment: String): Unit =
    result(entailment) should matchPattern:
      case Right(Contradiction(_, _)) =>

  def contradiction(entailment: String, varName: String): Unit =
    contradiction(entailment, Contradiction.fromPropVarName(varName))

  def contradiction(entailment: String, clause1: Clause, clause2: Clause): Unit =
    contradiction(entailment, Contradiction(clause1, clause2))

  def contradiction(entailment: String, contradiction: Contradiction): Unit =
    result(entailment) should be(Right(contradiction))

  def exhaustion(entailment: String): Unit =
    result(entailment) should be(Right(Exhaustion))

object BaseProofSpec:
  def result(entailment: String): Either[String, Step] =
    Prover
      .prove(entailment)
      .map: (_, proof) =>
        Proof.toSteps(proof).init.last
