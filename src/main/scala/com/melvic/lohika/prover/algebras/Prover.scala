package com.melvic.lohika.prover.algebras

import cats.*
import cats.implicits.*
import com.melvic.lohika.Cnf.Clause
import com.melvic.lohika.{Clauses, Cnf}
import com.melvic.lohika.formula.Formula

trait Prover[F[_]]:
  def splitAllIntoClauses(cnfs: List[Cnf]): F[Clauses]

  def splitIntoClauses(cnf: Cnf): F[Clauses] =
    splitAllIntoClauses(cnf :: Nil)

  def convertAllToCnfs(formulae: List[Formula]): F[List[Cnf]]

  def convertToCnf(formula: Formula)(implicit functor: Functor[F]): F[Cnf] =
    convertAllToCnfs(formula :: Nil).map(_.head)

  def updateClauseSet(clauseSet: Clauses, newClauses: Clauses): F[Clauses]

  def transform(lhs: Formula, rhs: Formula): F[Formula]

  def applyResolution(clauseSet: Clauses): F[Clauses]

  def applyResolutionOnPair(clause1: Clause, clause2: Clause): F[Option[Clauses]]

  def describe(description: String): F[Unit]

object Prover:
  def apply[F[_]](using prover: Prover[F]): Prover[F] = prover
