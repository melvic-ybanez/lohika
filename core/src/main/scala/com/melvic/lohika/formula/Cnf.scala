package com.melvic.lohika.formula

import cats.*
import cats.implicits.*
import com.melvic.lohika.formula.Cnf.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.Formatter

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
  final case class COr(literals: List[Literal])
  final case class CNot(atomic: PredicateApp)

  type Clause = COr | Literal
  type Literal = PredicateApp | CNot

sealed trait CnfGivens:
  given [C <: Cnf](using Formatter): Show[C] = Show.show(Formula.fromCnf(_).show)

  given Conversion[String, Clause] = Formula.toCnf(_) match
    case clause: Clause => clause
    case _              => COr(Nil)
