package com.melvic.lohika

import com.melvic.lohika.ProofSpec.result
import com.melvic.lohika.prover.algebras.Prover.{Contradiction, Exhaustion, ResolutionResult}
import com.melvic.lohika.prover.interpreters.LiveProver.{Steps, given}
import com.melvic.lohika.prover.programs.ProverProgram
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ProofSpec extends AnyFlatSpec with should.Matchers:
  "A" should "be provable from A | B" in:
    contradiction("A |= A | B", "A")

  "A & C" should "not be provable from A & !C" in:
    exhaustion("A & !C |= A & C")

  "A | B" should "be provable from A & C" in:
    contradiction("A & C |= A | B", "A")

  "C" should "be provable from A => B, B => C, and A" in:
    contradiction("A => B, B => C, A |= C", "!A")

  "P | R" should "be provable from P | Q, !Q | R" in:
    contradiction("P | Q, !Q | R |= P | R", "Q")

  "A | C" should "not be provable from A => B, and B | C" in:
    exhaustion("A => B, B | C |= A | C")

  "Q" should "be provable from !P | Q and P" in:
    contradiction("!P | Q, P |= Q", "P")

  "R" should "be provable from P | Q, !Q | R, and !P" in:
    contradiction("P | Q, !Q | R, !P |= R", "P")

  "B | C" should "be provable from A | B, !A" in:
    contradiction("A | B, !A |= B | C", "!A")

  "A | !C" should "be provable from A => B and B => C" in:
    result("A => B, B => C |= A | !C") should be(Right(Exhaustion))

  "P | !P" should "be a tautology" in:
    contradiction("P | !P", "!P")

  "(P & Q) => P" should "be a tautology" in:
    contradiction("P | !P", "!P")

  "(P | Q) & (!Q => R) => (P => R)" should "not be provable from P | Q and !Q => R" in:
    exhaustion("P | Q, !Q => R |= (P | Q) & (!Q => R) => (P => R)")

  "P => Q, Q <=> R, P |= R" should "hold" in:
    contradiction("P => Q, Q <=> R, P |= R", "R")

  "A => B, B => C, !C |= !A" should "hold" in:
    contradiction("A => B, B => C, !C |= !A", "!C")

  "P <=> Q, Q => R, P |= R" should "hold" in:
    contradiction("P <=> Q, Q => R, P |= R", "R")

  "P => Q, Q <=> R, !R |= !P" should "hold" in:
    contradiction("P => Q, Q <=> R, !R |= !P", "R")

  "P <=> Q, Q => R, R |= P" should "not hold" in:
    exhaustion("P <=> Q, Q => R, R |= P")

  "P => Q, R => P, R |= Q" should "hold" in:
    contradiction("P => Q, R => P, R |= Q", "!P")

  "P => Q, Q => R, R => S, P |= S" should "hold" in:
    contradiction("P => Q, Q => R, R => S, P |= S", "R")

  // [Showcase]
  "A => B, B => C, C => D, !D |= !A" should "hold" in:
    contradiction("A => B, B => C, C => D, !D |= !A", "!A")

  // [Showcase]
  "A => B, B => C, A, !D |= C | D" should "hold" in:
    contradiction("A => B, B => C, A, !D |= C | D", "!A")

  "X => Y, Y => Z, Z => W, W |= X" should "not hold" in:
    exhaustion("X => Y, Y => Z, Z => W, W |= X")

  "Non-provable first-order entailments" should "result to exhaustion" in:
    exhaustion("E:xP(x) |= P(a)")

  def contradiction(entailment: String, varName: String): Unit =
    result(entailment) should be(Right(Contradiction.fromPropVarName(varName)))

  def exhaustion(entailment: String): Unit =
    result(entailment) should be(Right(Exhaustion))

object ProofSpec:
  def result(entailment: String): Either[String, ResolutionResult] =
    ProverProgram.prove[Steps](entailment).run.map(_._2._2)
