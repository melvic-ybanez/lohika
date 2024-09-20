package com.melvic.lohika.tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import ProofSpec.result
import com.melvic.lohika.Cnf.*
import com.melvic.lohika.prover.algebras.Prover.{Contradiction, Exhaustion, ResolutionResult}
import com.melvic.lohika.prover.interpreters.LiveProver.{Steps, given}
import com.melvic.lohika.prover.programs.ProverProgram

class ProofSpec extends AnyFlatSpec with should.Matchers:
  "A" should "be provable from A | B" in:
    result("A |= A | B") should be(Right(Contradiction(CVar("A"), CNot(CVar("A")))))

  "A & C" should "not be provable from A & !C" in:
    result("A & !C |= A & C") should be(Right(Exhaustion))

  "A | B" should "be provable from A & C" in:
    result("A & C |= A | B") should be(Right(Contradiction(CVar("A"), CNot(CVar("A")))))

  "C" should "be provable from A => B, B => C, and A" in:
    result("A => B, B => C, A |= C") should be(Right(Contradiction(CNot(CVar("C")), CVar("C"))))

  "P | R" should "be provable from P | Q, !Q | R" in:
    result("P | Q, !Q | R |= P | R") should be(Right(Contradiction(CVar("R"), CNot(CVar("R")))))

  "A | C" should "not be provable from A => B, and B | C" in:
    result("A => B, B | C |= A | C") should be(Right(Exhaustion))

  "Q" should "be provable from !P | Q and P" in:
    result("!P | Q, P |= Q") should be(Right(Contradiction(CNot(CVar("P")), CVar("P"))))

  "R" should "be provable from P | Q, !Q | R, and !P" in:
    result("P | Q, !Q | R, !P |= R") should be(Right(Contradiction(CVar("R"), CNot(CVar("R")))))

  "B | C" should "be provable from A | B, !A" in:
    result("A | B, !A |= B | C") should be(Right(Contradiction(CVar("B"), CNot(CVar("B")))))

  "A | !C" should "be provable from A => B and B => C" in:
    result("A => B, B => C |= A | !C") should be(Right(Exhaustion))

  "P | !P" should "be a tautology" in:
    result("P | !P") should be(Right(Contradiction(CNot(CVar("P")), CVar("P"))))

  "(P & Q) => P" should "be a tautology" in:
    result("P | !P") should be(Right(Contradiction(CNot(CVar("P")), CVar("P"))))

  "(P | Q) & (!Q => R) => (P => R)" should "not be provable from P | Q and !Q => R" in:
    result("P | Q, !Q => R |= (P | Q) & (!Q => R) => (P => R)") should be(Right(Exhaustion))

object ProofSpec:
  def result(entailment: String): Either[String, ResolutionResult] =
    ProverProgram.prove[Steps](entailment).run.map(_._2._2)
