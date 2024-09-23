package com.melvic.lohika.prover.algebras

import cats.*
import cats.implicits.*
import com.melvic.lohika.Cnf.{CNot, CVar, Clause}
import Prover.ResolutionResult
import com.melvic.lohika.{Clauses, Cnf, Entailment}
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.parsers.Lexemes

trait Prover[F[_]]:
  def parseEntailment(rawEntailment: String): F[Entailment]

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

  object Contradiction:
    /**
     * Creates a contradiction (e.g. P & !P) from the variable name. If the input is in the form
     * `!P`, the resulting contradiction is `!P & P`. Otherwise, it is `P & !P`
     */
    def fromVarName(varName: String): Contradiction =
      if varName.startsWith(Lexemes.Not) then
        val variable = CVar(varName.tail)
        Contradiction(CNot(variable), variable)
      else
        val variable = CVar(varName)
        Contradiction(variable, CNot(variable))

  def apply[F[_]](using prover: Prover[F]): Prover[F] = prover
