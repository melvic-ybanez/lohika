package com.melvic.lohika.prover.programs

import cats.*
import cats.implicits.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.prover.algebras.Prover
import com.melvic.lohika.prover.algebras.Prover.*
import com.melvic.lohika.{Clauses, Problem}

object ProverProgram:
  import com.melvic.lohika.Givens.given

  def prove[F[_]: Prover: Monad](
      rawAssumptions: String,
      rawProposition: String
  ): F[ResolutionResult] =
    for
      Problem(assumptions, proposition) <- Prover[F].parseProblem(rawAssumptions, rawProposition)
      _                                 <- Prover[F].write("Convert all assumptions to CNFs")
      assumptionCnfs                    <- Prover[F].convertAllToCnfs(assumptions)
      assumptionClauses                 <- Prover[F].splitAllIntoClauses(assumptionCnfs)
      clauses            <- Prover[F].updateClauseSet(Clauses.empty, assumptionClauses)
      _                  <- Prover[F].write("Negate the proposition")
      negatedProp        <- Prover[F].transform(proposition, !proposition)
      _                  <- Prover[F].write("Convert the negated proposition into CNF")
      negatedPropCnf     <- Prover[F].convertToCnf(negatedProp)
      negatedPropClauses <- Prover[F].splitIntoClauses(negatedPropCnf)
      clauses            <- Prover[F].updateClauseSet(clauses, negatedPropClauses)
      result             <- resolveRecursively(assumptions, proposition, clauses)
    yield result

  def resolveRecursively[F[_]: Prover: Monad](
      assumptions: List[Formula],
      proposition: Formula,
      clauseSet: Clauses
  ): F[ResolutionResult] =
    for
      resolutionResult <- Prover[F].resolve(clauseSet)
      result <- resolutionResult match
        case Exhaustion =>
          for
            _ <- Prover[F].write("Resolution options exhausted.")
            _ <- conclusion(assumptions, proposition, false)
          yield Exhaustion
        case contradiction @ Contradiction(clause1, clause2) =>
          for
            _ <- Prover[F].write(show"A contradiction is found: $clause1 and $clause2")
            _ <- conclusion(assumptions, proposition, true)
          yield contradiction
        case Derive(left, right, clause) =>
          for
            _            <- Prover[F].write(show"$left and $right resolves to $clause")
            newClauseSet <- Prover[F].updateClauseSet(clauseSet, Clauses.one(clause))
            result       <- resolveRecursively(assumptions, proposition, newClauseSet)
          yield result
    yield result

  def conclusion[F[_]: Prover](
      assumptions: List[Formula],
      proposition: Formula,
      provable: Boolean
  ): F[Unit] =
    val not = if provable then "" else " not"
    if assumptions.isEmpty then Prover[F].write(show"Conclusion: $proposition is$not a tautology.")
    else Prover[F].write(show"Conclusion: $proposition is$not provable from $assumptions")
