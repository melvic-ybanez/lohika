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

object Clauses extends ClausesImplicits:
  def apply(clause: Clause*): Clauses =
    Clauses(clause.toList)

  def one(clause: Clause): Clauses =
    Clauses(clause :: Nil)

  def empty: Clauses = Clauses(Nil)

  def fromCnf: Cnf => Clauses =
    case CAnd(clauses)  => Clauses(clauses)
    case clause: Clause => one(clause)

  def fromCnfs: List[Cnf] => Clauses =
    _.map(fromCnf).foldLeft(Clauses.empty): (acc, clause) =>
      acc ++ clause

  def fromFormula: Formula => Clauses =
    Cnf.fromFormula andThen fromCnf

  def fromAllFormulae: List[Formula] => Clauses =
    fms => fromCnfs(fms.map(Cnf.fromFormula))

sealed trait ClausesImplicits:
  given showClauses: Show[Clauses] = Show.show:
    case Clauses(underlying) =>
      s"Clauses(${underlying.map(_.show).mkString(", ")})"
