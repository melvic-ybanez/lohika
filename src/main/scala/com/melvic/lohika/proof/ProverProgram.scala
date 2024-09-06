package com.melvic.lohika.proof

import cats.*
import cats.implicits.*
import com.melvic.lohika.Problem

object ProverProgram:
  def prove[F[_]: Prover: Monad]: Problem => F[Boolean] =
    case Problem(assumptions, proposition) =>
      for
        assumptionCnfs     <- Prover[F].convertAllToCnfs(assumptions)
        assumptionClauses  <- Prover[F].splitAllIntoClauses(assumptionCnfs)
        clauses            <- Prover[F].updateClauses(Clauses.empty, assumptionClauses)
        negatedProp        <- Prover[F].negateProposition(proposition)
        negatedPropCnf     <- Prover[F].convertToCnf(negatedProp)
        negatedPropClauses <- Prover[F].splitIntoClauses(negatedPropCnf)
        clauses            <- Prover[F].updateClauses(clauses, negatedPropClauses)
        result             <- applyResolution(clauses)
      yield result

  def applyResolution[F[_]: Prover: Monad](clauses: Clauses): F[Boolean] =
    for {
      updatedClauses <- Prover[F].applyResolution(clauses)
      result <-
        if updatedClauses.isEmpty then true.pure
        else applyResolution(updatedClauses)
    } yield result
