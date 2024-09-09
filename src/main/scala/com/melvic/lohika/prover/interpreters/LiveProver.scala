package com.melvic.lohika.prover.interpreters

import cats.implicits.*
import cats.data.Writer
import com.melvic.lohika.Cnf.Clause
import com.melvic.lohika.{Clauses, Cnf, Equivalence}
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.prover.algebras.Prover

object LiveProver:
  import com.melvic.lohika.Givens.given

  type Step[X] = Writer[List[String], X]

  given liveProver: Prover[Step] with
    override def splitAllIntoClauses(cnfs: List[Cnf]): Step[Clauses] =
      step(s"Split all the clauses from ${cnfs.show}.", Clauses.fromCnfs(cnfs))

    override def convertAllToCnfs(formulae: List[Formula]): Step[List[Cnf]] =
      formulae.foldLeft(Writer(List(s"Convert ${formulae.show} to CNFs"), List.empty[Cnf])):
        (step, formula) =>
          step.flatMap: cnfs =>
            val cnf = Cnf.fromFormula(formula)
            Writer("* " + Equivalence(formula, cnf).show :: Nil, cnf :: cnfs)

    override def updateClauseSet(clauseSet: Clauses, newClauses: Clauses): Step[Clauses] =
      val newClauseSet = clauseSet ++ newClauses
      step(
        s"Add ${newClauses.show} to the clause set. Here's the updated clause set: ${newClauseSet.show}",
        newClauseSet
      )

    override def transform(lhs: Formula, rhs: Formula): Step[Formula] =
      step(s"${lhs.show} becomes ${rhs.show}", rhs)

    override def applyResolution(clauseSet: Clauses): Step[Clauses] = ???

    override def applyResolutionOnPair(clause1: Clause, clause2: Clause): Step[Option[Clauses]] =
      ???

    override def describe(description: String): Step[Unit] =
      step(description, ())

  def step[A](description: String, value: A): Step[A] =
    Writer(description :: Nil, value)

  def step[A](value: A): Step[A] = Writer(Nil, value)
