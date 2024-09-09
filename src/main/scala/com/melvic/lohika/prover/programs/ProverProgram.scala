package com.melvic.lohika.prover.programs

import cats.*
import cats.implicits.*
import com.melvic.lohika.{Clauses, Problem}
import com.melvic.lohika.prover.algebras.Prover

object ProverProgram:
  def prove[F[_]: Prover: Monad]: Problem => F[Boolean] =
    case Problem(assumptions, proposition) =>
      for
        _                  <- Prover[F].describe("Convert all assumptions to CNFs")
        assumptionCnfs     <- Prover[F].convertAllToCnfs(assumptions)
        assumptionClauses  <- Prover[F].splitAllIntoClauses(assumptionCnfs)
        clauses            <- Prover[F].updateClauseSet(Clauses.empty, assumptionClauses)
        _                  <- Prover[F].describe("Negate the proposition")
        negatedProp        <- Prover[F].transform(proposition, !proposition)
        _                  <- Prover[F].describe("Convert the negated proposition into CNF")
        negatedPropCnf     <- Prover[F].convertToCnf(negatedProp)
        negatedPropClauses <- Prover[F].splitIntoClauses(negatedPropCnf)
        clauses            <- Prover[F].updateClauseSet(clauses, negatedPropClauses)
        result             <- applyResolution(clauses)
      yield result

  def applyResolution[F[_]: Prover: Monad](clauseSet: Clauses): F[Boolean] = ???

