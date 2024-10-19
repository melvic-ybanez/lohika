package com.melvic.lohika.prover.programs

import cats.*
import cats.implicits.*
import com.melvic.lohika.Formatter.*
import com.melvic.lohika.formula.Formula.SkolemSuffix
import com.melvic.lohika.formula.{Clauses, Formula}
import com.melvic.lohika.meta.Entailment
import com.melvic.lohika.prover.algebras.Prover
import com.melvic.lohika.prover.algebras.Prover.*
import com.melvic.lohika.{Formatter, Links}

import java.text.NumberFormat.Style

object ProverProgram:
  import com.melvic.lohika.Givens.given

  def prove[F[_]: Prover: Monad](
      rawEntailment: String
  )(using Formatter): F[(Entailment, ResolutionResult)] =
    for
      entailment @ Entailment(premises, conclusion) <- Prover[F].parseEntailment(rawEntailment)
      _ <- Prover[F].write(
        s"Convert all premises into their ${"conjunctive normal forms (CNFs)".link(Links.Cnf)}:"
      )
      premiseCnfs    <- Prover[F].convertAllToCnfs(premises)(using SkolemSuffix(1))
      premiseClauses <- Prover[F].splitAllIntoClauses(premiseCnfs)
      clauses        <- Prover[F].updateClauseSet(Clauses.empty, premiseClauses)
      _ <- Prover[F].write(
        s"Negate the conclusion (${"proof by contradiction".link(Links.ProofByContradiction)}):"
      )
      negatedConclusion <- Prover[F].transform(conclusion, !Formula.addImpliedForall(conclusion))
      _                 <- Prover[F].write("Convert the negated conclusion into CNF:")
      given SkolemSuffix = SkolemSuffix(premises.length + 1)
      negatedPropCnf     <- Prover[F].convertToCnf(negatedConclusion)
      negatedPropClauses <- Prover[F].splitIntoClauses(negatedPropCnf)
      clauses            <- Prover[F].updateClauseSet(clauses, negatedPropClauses)
      result             <- resolveRecursively(premises, conclusion, clauses)
    yield (entailment, result)

  def resolveRecursively[F[_]: Prover: Monad](
      premises: List[Formula],
      conclusion: Formula,
      clauseSet: Clauses
  )(using Formatter): F[ResolutionResult] =
    for
      resolutionResult <- Prover[F].resolve(clauseSet)
      result <- resolutionResult match
        case Exhaustion =>
          for
            _ <- Prover[F].write(s"Resolution options ${"exhausted".emphasize}.")
            _ <- proofResult(premises, conclusion, false)
          yield Exhaustion
        case contradiction @ Contradiction(clause1, clause2) =>
          for
            _ <- Prover[F].write(
              show"A ${"contradiction".emphasize} is found: $clause1 and $clause2"
            )
            _ <- proofResult(premises, conclusion, true)
          yield contradiction
        case Derived(left, right, clause) =>
          for
            _            <- Prover[F].write(show"$left and $right resolves to $clause")
            newClauseSet <- Prover[F].updateClauseSet(clauseSet, Clauses.one(clause))
            result       <- resolveRecursively(premises, conclusion, newClauseSet)
          yield result
    yield result

  def proofResult[F[_]: Prover](
      premises: List[Formula],
      conclusion: Formula,
      provable: Boolean
  )(using Formatter): F[Unit] =
    val followsString = if provable then "follows" else "does not follow"
    val notString = if provable then "" else " not"
    val resultPrefix = "Proof Result".strong + ": "

    if premises.isEmpty then
      Prover[F].write(
        show"$resultPrefix$conclusion is$notString a ${"tautology".link(Links.Tautology)}."
      )
    else Prover[F].write(show"$resultPrefix$conclusion $followsString from $premises")
