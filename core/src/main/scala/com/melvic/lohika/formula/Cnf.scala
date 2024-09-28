package com.melvic.lohika.formula

import cats.*
import cats.implicits.*
import com.melvic.lohika.formula.Cnf.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.Formatter
import Converter.*
import com.melvic.lohika.formula.Nnf.{moveNegationsInside, simplifyNegations}

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

  def fromFormula: Formula => Cnf =
    fromFormulaUntyped andThen:
      case or: Or if or.components.forall(isLiteral) =>
        COr(or.components.map(fromFormula(_).asInstanceOf[Literal]))
      case and: And if and.components.forall(isClause) =>
        CAnd(and.components.map(fromFormula(_).asInstanceOf[Clause]))
      case Not(Var(name)) => CNot(CVar(name))
      case Var(name)      => CVar(name)
      case True           => CTrue
      case False          => CFalse
      case fm             => CAnd(Nil)

  def toFormula: Cnf => Formula =
    case CAnd(p :: q :: rs) => And(toFormula(p), toFormula(q), rs.map(toFormula))
    case CAnd(p :: Nil)     => toFormula(p)
    case CAnd(Nil)          => True
    case COr(p :: q :: rs)  => Or(toFormula(p), toFormula(q), rs.map(toFormula))
    case COr(p :: Nil)      => toFormula(p)
    case COr(Nil)           => False
    case CNot(p)            => !toFormula(p)
    case CVar(name)         => Var(name)
    case CTrue              => True
    case CFalse             => False

  def fromFormulaUntyped: Endo[Formula] =
    eliminateBiconditionals andThen
      eliminateImplications andThen
      moveNegationsInside andThen
      distributeOrOverAnds andThen
      simplifyNegations andThen
      flattenOrsAndAnds

sealed trait CnfGivens:
  given [C <: Cnf](using Formatter): Show[C] = Show.show(toFormula(_).show)

  given Conversion[String, Clause] = fromFormula(_) match
    case clause: Clause => clause
    case _              => COr(Nil)
