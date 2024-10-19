package com.melvic.lohika.prover.algebras

import cats.*
import cats.implicits.*
import com.melvic.lohika.formula.Cnf.{CNot, Clause}
import com.melvic.lohika.formula.Formula.{SkolemSuffix, PredicateApp}
import com.melvic.lohika.formula.{Clauses, Cnf, Formula}
import com.melvic.lohika.meta.Entailment
import com.melvic.lohika.parsers.Lexemes
import com.melvic.lohika.prover.algebras.Prover.ResolutionResult

trait Prover[F[_]]:
  def parseEntailment(rawEntailment: String): F[Entailment]

  def splitAllIntoClauses(cnfs: List[Cnf]): F[Clauses]

  def splitIntoClauses(cnf: Cnf): F[Clauses] =
    splitAllIntoClauses(cnf :: Nil)

  def convertAllToCnfs(formulae: List[Formula])(using SkolemSuffix): F[List[Cnf]]

  def convertToCnf(formula: Formula)(using Functor[F], SkolemSuffix): F[Cnf] =
    convertAllToCnfs(formula :: Nil).map(_.head)

  def updateClauseSet(clauseSet: Clauses, newClauses: Clauses): F[Clauses]

  def transform(lhs: Formula, rhs: Formula): F[Formula]

  def resolve(clauseSet: Clauses): F[ResolutionResult]

  def write(description: String): F[Unit]

object Prover:
  type ResolutionResult = Derived | Contradiction | Exhaustion.type

  final case class Derived(left: Clause, right: Clause, result: Clause)
  final case class Contradiction(clause1: Clause, clause2: Clause)
  case object Exhaustion

  object Contradiction:
    /**
     * Creates a contradiction (e.g. P & !P) from the variable name. If the input is in the form
     * `!P`, the resulting contradiction is `!P & P`. Otherwise, it is `P & !P`
     */
    def fromPropVarName(varName: String): Contradiction =
      if varName.startsWith(Lexemes.Not) then
        val variable = PredicateApp.nullary(varName.tail)
        Contradiction(CNot(variable), variable)
      else
        val variable = PredicateApp.nullary(varName)
        Contradiction(variable, CNot(variable))

  def apply[F[_]](using prover: Prover[F]): Prover[F] = prover
