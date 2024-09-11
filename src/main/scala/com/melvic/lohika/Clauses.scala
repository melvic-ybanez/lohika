package com.melvic.lohika

import cats.*
import cats.implicits.*
import com.melvic.lohika.Cnf.*
import com.melvic.lohika.formula.Formula

import scala.annotation.targetName

final case class Clauses(underlying: Set[Clause]):
  import Formula.{given, *}
  
  @targetName("concat")
  def ++(that: Clauses): Clauses =
    Clauses(this.underlying ++ that.underlying)

  def contains(clause: Clause): Boolean =
    underlying.exists(thisClause => Cnf.toFormula(thisClause) === Cnf.toFormula(clause))

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
    Cnf.fromFormula andThen fromCnf

  def fromAllFormulae: List[Formula] => Clauses =
    fms => fromCnfs(fms.map(Cnf.fromFormula))

sealed trait ClausesGivens:
  import Givens.given

  given showClauses: Show[Clauses] = Show.show:
    case Clauses(underlying) => underlying.toList.show
