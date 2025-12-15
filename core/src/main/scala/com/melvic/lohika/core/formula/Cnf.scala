package com.melvic.lohika.core.formula

import cats.*
import cats.implicits.*
import com.melvic.lohika.core.Formatter
import com.melvic.lohika.core.Formatter
import Cnf.*
import Formula.*
import com.melvic.lohika.core.expression.Expression

/**
 * Conjunctive Normal Form
 */
type Cnf = CAnd | Clause

/**
 * Note: The names of the types here are based on the terminology in propositional logic, as opposed
 * to first-order logic.
 */
object Cnf extends CnfGivens:
  final case class CAnd(clauses: List[Clause]) // TODO: Require at least one clause
  final case class COr(literals: List[CLiteral]) // TODO: Require at least one literal

  final case class CNot(atomic: PredicateApp)

  type Clause = COr | CLiteral
  type CLiteral = PredicateApp | CNot

  def prettyPrint(cnf: Cnf): String =
    Expression.prettyPrint(Formula.fromCnf(cnf))

sealed trait CnfGivens:
  given [C <: Cnf](using Formatter): Show[C] = Show.show(Formula.fromCnf(_).show)

  given Conversion[String, Clause] = Formula.toCnf(_) match
    case clause: Clause => clause
    case _              => COr(Nil)
