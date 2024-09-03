package com.melvic.lohika.proof

import cats.data.Writer
import com.melvic.lohika.Problem
import com.melvic.lohika.proof.Step.*

object Prover:
  def prove: Problem => Writer[List[Step], Unit] =
    case Problem(assumptions, proposition) =>
      val assumptionCnfs = assumptions.map(Cnf.fromFormula)
 
      for
        _ <- ConvertToCnfs("Convert each assumption to CNF", assumptions.zip(assumptionCnfs)).log
        initClauses = Clauses.fromCnfs(assumptionCnfs)
        _ <- UpdateClauses("List all the clauses", initClauses).log
        negatedProposition = !proposition
        _ <- ConvertFormulae(
          "Negate the proposition",
          (proposition -> negatedProposition) :: Nil
        ).log
        _ <- ConvertToCnfs(
          "Convert the negated proposition to CNF",
          (negatedProposition -> Cnf.fromFormula(negatedProposition)) :: Nil
        ).log
        _ <- UpdateClauses("Add the result to the set of clauses", initClauses).log
      yield ()
