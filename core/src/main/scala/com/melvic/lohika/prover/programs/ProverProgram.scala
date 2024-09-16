package com.melvic.lohika.prover.programs

import cats.*
import cats.implicits.*
import com.melvic.lohika.prover.algebras.Prover
import Prover.*
import com.melvic.lohika.{Clauses, Formatter, Links, Entailment}
import com.melvic.lohika.formula.Formula
import Formatter.*

import java.text.NumberFormat.Style

object ProverProgram:
  import com.melvic.lohika.Givens.given

  def prove[F[_]: Prover: Monad](rawEntailment: String)(using Formatter): F[ResolutionResult] =
    for
      Entailment(premises, conclusion) <- Prover[F].parseEntailment(rawEntailment)
      _ <- Prover[F].write(
        s"Convert all premises into their ${"conjunctive normal forms (CNFs)".link(Links.Cnf)}:"
      )
      premiseCnfs        <- Prover[F].convertAllToCnfs(premises)
      premiseClauses     <- Prover[F].splitAllIntoClauses(premiseCnfs)
      clauses            <- Prover[F].updateClauseSet(Clauses.empty, premiseClauses)
      _                  <- Prover[F].write("Negate the conclusion:")
      negatedConclusion  <- Prover[F].transform(conclusion, !conclusion)
      _                  <- Prover[F].write("Convert the negated conclusion into CNF:")
      negatedPropCnf     <- Prover[F].convertToCnf(negatedConclusion)
      negatedPropClauses <- Prover[F].splitIntoClauses(negatedPropCnf)
      clauses            <- Prover[F].updateClauseSet(clauses, negatedPropClauses)
      result             <- resolveRecursively(premises, conclusion, clauses)
    yield result

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
        case Derive(left, right, clause) =>
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
    val resultPrefix = "Proof Result".strong + ": "
    
    if premises.isEmpty then
      Prover[F].write(
        show"$resultPrefix$conclusion is$followsString a ${"tautology".link(Links.Tautology)}."
      )
    else Prover[F].write(show"$resultPrefix$conclusion $followsString from $premises")
