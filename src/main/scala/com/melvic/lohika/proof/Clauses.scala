package com.melvic.lohika.proof

import cats.*
import cats.implicits.*
import com.melvic.lohika.Problem
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.proof.Cnf.*

import scala.annotation.targetName

final case class Clauses(underlying: List[Clause]):
  @targetName("concat")
  def ++(that: Clauses): Clauses =
    Clauses(this.underlying ++ that.underlying)

object Clauses extends ClauseImplicits:
  def apply(clause: Clause*): Clauses =
    Clauses(clause.toList)

  def one(clause: Clause): Clauses =
    Clauses(clause :: Nil)

  def empty: Clauses = Clauses(Nil)

  def fromCnf: Cnf => Clauses =
    case CAnd(clauses)  => Clauses(clauses)
    case clause: Clause => one(clause)

  def fromFormula: Formula => Clauses =
    Cnf.fromFormula andThen fromCnf

  def fromProblem: Problem => Clauses =
    case Problem(assumptions, proposition) =>
      val assumptionClauses = assumptions
        .map(fromFormula)
        .foldLeft(Clauses.empty): (acc, clause) =>
          acc ++ clause
      assumptionClauses ++ Clauses.fromFormula(!proposition)

sealed trait ClauseImplicits:
  given showClauses: Show[Clauses] = Show.show:
    case Clauses(underlying) =>
      s"Clauses(${underlying.map(_.show).mkString(", ")})"
