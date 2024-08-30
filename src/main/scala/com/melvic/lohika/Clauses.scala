package com.melvic.lohika

import cats.*
import cats.implicits.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*

final case class Clauses(underlying: List[Formula]):
  def ++(that: Clauses): Clauses =
    Clauses(this.underlying ++ that.underlying)

object Clauses extends Implicits:
  def one(formula: Formula): Clauses =
    Clauses(formula :: Nil)

  def empty: Clauses = Clauses(Nil)

  def fromFormula: Formula => Clauses =
    case and: And           => Clauses(and.components)
    case fm if isClause(fm) => one(fm)
    case _                  => empty

  def fromProblem: Problem => Clauses =
    case Problem(assumptions, proposition) =>
      val assumptionClauses = assumptions
        .map(fm => Clauses.fromFormula(Cnf.fromFormula(fm)))
        .foldLeft(Clauses.empty): (acc, clause) =>
          acc ++ clause
      val negatedPropositionCnf = Cnf.fromFormula(!proposition)
      assumptionClauses ++ Clauses.fromFormula(negatedPropositionCnf)

trait Implicits:
  given show: Show[Clauses] = Show.show:
    case Clauses(underlying) =>
      s"Clauses(${underlying.map(_.show).mkString(", ")})"
