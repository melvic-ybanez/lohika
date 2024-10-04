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

object Cnf extends CnfGivens:
  final case class CAnd(clauses: List[Clause])
  final case class COr(literals: List[Literal])
  final case class CNot(variable: CVar | Constant)
  final case class CVar(name: String)
  case object CTrue
  case object CFalse

  type Clause = COr | Literal
  type Literal = CVar | Constant | CNot
  type Constant = CTrue.type | CFalse.type

sealed trait CnfGivens:
  given [C <: Cnf](using Formatter): Show[C] = Show.show(Formula.fromCnf(_).show)

  given Conversion[String, Clause] = Formula.toCnf(_) match
    case clause: Clause => clause
    case _              => COr(Nil)
