package com.melvic.lohika.core.formula

import cats.*
import cats.implicits.*
import com.melvic.lohika.core.Formatter
import com.melvic.lohika.core.Formatter
import Cnf.*
import Formula.*

/**
 * Conjunctive Normal Form
 */
type Cnf = CAnd | Clause

/**
 * Note: The names of the types here are based on the terminology in propositional logic, as opposed
 * to first-order logic. Some things might change after Lohika fully supports first-order logic
 */
object Cnf extends CnfGivens:
  final case class CAnd(clauses: List[Clause])
  final case class COr(literals: List[CLiteral])
  final case class CNot(atomic: PredicateApp)

  type Clause = COr | CLiteral
  type CLiteral = PredicateApp | CNot

sealed trait CnfGivens:
  given [C <: Cnf](using Formatter): Show[C] = Show.show(Formula.fromCnf(_).show)

  given Conversion[String, Clause] = Formula.toCnf(_) match
    case clause: Clause => clause
    case _              => COr(Nil)
