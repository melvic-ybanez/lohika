package com.melvic.lohika.prover.programs

import cats.*
import cats.implicits.*
import com.melvic.lohika.Cnf.Clause
import com.melvic.lohika.{Clauses, Problem}
import com.melvic.lohika.prover.algebras.Prover
import com.melvic.lohika.prover.algebras.Prover.*

object ProverProgram:
  def prove[F[_]: Prover: Monad](rawAssumptions: String, rawProposition: String): F[Boolean] =
    for
      Problem(assumptions, proposition) <- Prover[F].parseProblem(rawAssumptions, rawProposition)
      _                                 <- Prover[F].describe("Convert all assumptions to CNFs")
      assumptionCnfs                    <- Prover[F].convertAllToCnfs(assumptions)
      assumptionClauses                 <- Prover[F].splitAllIntoClauses(assumptionCnfs)
      clauses            <- Prover[F].updateClauseSet(Clauses.empty, assumptionClauses)
      _                  <- Prover[F].describe("Negate the proposition")
      negatedProp        <- Prover[F].transform(proposition, !proposition)
      _                  <- Prover[F].describe("Convert the negated proposition into CNF")
      negatedPropCnf     <- Prover[F].convertToCnf(negatedProp)
      negatedPropClauses <- Prover[F].splitIntoClauses(negatedPropCnf)
      clauses            <- Prover[F].updateClauseSet(clauses, negatedPropClauses)
      result             <- resolveRecursively(clauses)
    yield result

  def resolveRecursively[F[_]: Prover: Monad](clauseSet: Clauses): F[Boolean] =
    for
      resolutionResult <- Prover[F].resolve(clauseSet)
      result <- resolutionResult match
        case Exhaustion          => false.pure
        case Contradiction(_, _) => false.pure
        case NewClause(clause)   => resolveRecursively(clauseSet + clause)
    yield result
