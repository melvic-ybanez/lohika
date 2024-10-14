package com.melvic.lohika.formula

import cats.*
import cats.implicits.*
import com.melvic.lohika.formula.Cnf.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.given
import com.melvic.lohika.*

import scala.annotation.targetName

opaque type Clauses = Set[Clause]

object Clauses extends ClausesGivens:
  def apply(clause: Clause*): Clauses = clause.toSet

  def one(clause: Clause): Clauses = Set(clause)

  def empty: Clauses = Set()

  def fromCnf: Cnf => Clauses =
    case CAnd(clauses)  => clauses.toSet
    case clause: Clause => one(clause)

  def fromCnfs: List[Cnf] => Clauses =
    _.map(fromCnf).combineAll

  def fromFormula: Formula => Clauses =
    Formula.toCnf andThen fromCnf

  def fromAllFormulae: List[Formula] => Clauses =
    fms => fromCnfs(fms.map(Formula.toCnf))

  extension (self: Clauses)
    @targetName("concat")
    def ++(other: Clauses): Clauses =
      self ++ other

    def contains(clause: Clause): Boolean =
      self.exists(thisClause => Formula.fromCnf(thisClause) === Formula.fromCnf(clause))

    def toList: List[Clause] =
      self.toList

    def isEmpty: Boolean = self.isEmpty

sealed trait ClausesGivens:
  import com.melvic.lohika.Givens.given

  given (using Formatter): Show[Clauses] = Show.show:
    _.toList.show
