package com.melvic.lohika.formula

import cats.*
import cats.implicits.*
import com.melvic.lohika.formula.Cnf.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.given
import com.melvic.lohika.*

import scala.annotation.targetName

final case class Clauses(underlying: Set[Clause]):
  @targetName("concat")
  def ++(that: Clauses): Clauses =
    Clauses(this.underlying ++ that.underlying)

  def contains(clause: Clause): Boolean =
    underlying.exists(thisClause => Formula.fromCnf(thisClause) === Formula.fromCnf(clause))

  def isEmpty: Boolean = underlying.isEmpty

object Clauses extends ClausesGivens:
  def apply(clause: Clause*): Clauses =
    Clauses(clause.toSet)

  def one(clause: Clause): Clauses =
    Clauses(Set(clause))

  def empty: Clauses = Clauses(Set())

  def fromCnf: Cnf => Clauses =
    case CAnd(clauses)  => Clauses(clauses.toSet)
    case clause: Clause => one(clause)

  def fromCnfs: List[Cnf] => Clauses =
    _.map(fromCnf).foldLeft(Clauses.empty): (acc, clause) =>
      acc ++ clause

  def fromFormula: Formula => Clauses =
    Formula.toCnf andThen fromCnf

  def fromAllFormulae: List[Formula] => Clauses =
    fms => fromCnfs(fms.map(Formula.toCnf))

sealed trait ClausesGivens:
  import Givens.given

  given (using Formatter): Show[Clauses] = Show.show:
    case Clauses(underlying) => underlying.toList.show
