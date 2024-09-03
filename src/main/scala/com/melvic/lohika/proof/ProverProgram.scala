package com.melvic.lohika.proof

import cats.*
import cats.implicits.*
import com.melvic.lohika.Problem

object ProverProgram:
  def prove[F[_]: Prover: Monad]: Problem => F[Unit] =
    case Problem(assumptions, proposition) =>
      for
        (_, assumptionCnfs) <- Prover[F].convertToCnfs(assumptions)
        clauses     <- Prover[F].updateClauses(Clauses.empty, Clauses.fromCnfs(assumptionCnfs))
        negatedProp <- Prover[F].negateProposition(proposition)
        clauses     <- Prover[F].updateClauses(clauses, Clauses.fromFormula(negatedProp))
      yield ()
