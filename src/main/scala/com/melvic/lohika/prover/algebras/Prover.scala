package com.melvic.lohika.prover.algebras

import cats.*
import cats.implicits.*
import com.melvic.lohika.Cnf.Clause
import com.melvic.lohika.{Clauses, Cnf, Problem}
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.prover.algebras.Prover.ResolutionResult

trait Prover[F[_]]:
  def parseProblem(assumptions: String, propsosition: String): F[Problem]

  def splitAllIntoClauses(cnfs: List[Cnf]): F[Clauses]

  def splitIntoClauses(cnf: Cnf): F[Clauses] =
    splitAllIntoClauses(cnf :: Nil)

  def convertAllToCnfs(formulae: List[Formula]): F[List[Cnf]]

  def convertToCnf(formula: Formula)(implicit functor: Functor[F]): F[Cnf] =
    convertAllToCnfs(formula :: Nil).map(_.head)

  def updateClauseSet(clauseSet: Clauses, newClauses: Clauses): F[Clauses]

  def transform(lhs: Formula, rhs: Formula): F[Formula]

  def resolve(clauseSet: Clauses): F[ResolutionResult]

  def write(description: String): F[Unit]

object Prover:
  type ResolutionResult = Derive | Contradiction | Exhaustion.type

  final case class Derive(left: Clause, right: Clause, result: Clause)
  final case class Contradiction(clause1: Clause, clause2: Clause)
  case object Exhaustion

  def apply[F[_]](using prover: Prover[F]): Prover[F] = prover
