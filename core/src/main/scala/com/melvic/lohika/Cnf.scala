package com.melvic.lohika

import cats.*
import cats.implicits.*
import com.melvic.lohika.formula.Formula.*
import Cnf.*
import com.melvic.lohika.formula.Formula

type Cnf = CAnd | Clause

object Cnf extends CnfImplicits:
  type ToCnfUntyped[A <: Formula] = A => Formula

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

  def fromFormulaUntyped: ToCnfUntyped[Formula] =
    case or: Or       => fromDisjunctionUntyped(or)
    case and: And     => fromConjunctionUntyped(and)
    case imply: Imply => fromImplicationUntyped(imply)
    case iff: Iff     => fromBiconditionalUntyped(iff)
    case not: Not     => fromNegationUntyped(not)
    case fm           => fm

  def fromDisjunctionUntyped: ToCnfUntyped[Or] =
    case or if isInCnf(or)          => or
    case Or(And(ap, aq, _), q, Nil) => fromFormulaUntyped((ap | q) & (aq | q))
    case Or(p: And, q, r :: rs)     => fromFormulaUntyped(Or(fromFormulaUntyped(p | q), r, rs))
    case or if or.components.exists(isAnd) =>
      or.components
        .find(isAnd)
        .fold(or): and =>
          fromFormulaUntyped(Or.fromList(and :: or.components.filterNot(_ == and)))
    case Or(p, q, rs) =>
      fromFormulaUntyped(
        Or.flatten(Or(fromFormulaUntyped(p), fromFormulaUntyped(q), rs.map(fromFormulaUntyped)))
      )

  def fromConjunctionUntyped: ToCnfUntyped[And] =
    case and if isInCnf(and) => and
    case And(p, q, rs) =>
      fromFormulaUntyped(
        And.flatten(And(fromFormulaUntyped(p), fromFormulaUntyped(q), rs.map(fromFormulaUntyped)))
      )

  def fromImplicationUntyped: ToCnfUntyped[Imply] =
    case Imply(p, q) => fromFormulaUntyped(!fromFormulaUntyped(p) | fromFormulaUntyped(q))

  def fromBiconditionalUntyped: ToCnfUntyped[Iff] =
    case Iff(p, q, Nil) =>
      fromFormulaUntyped(fromFormulaUntyped(p ==> q) & fromFormulaUntyped(q ==> p))
    case Iff(p, q, rs) =>
      val iffs = rs.foldLeft(List(p <==> q)):
        case (iffs @ (Iff(p, q, _) :: _), r) => (q <==> r) :: iffs
      fromFormulaUntyped(And.fromList(iffs.reverse))

  def fromNegationUntyped: ToCnfUntyped[Not] =
    case Not(Or(p, q, rs))  => fromFormulaUntyped(And(!p, !q, rs.map(!_)))
    case Not(And(p, q, rs)) => fromFormulaUntyped(Or(!p, !q, rs.map(!_)))
    case Not(Not(p))        => fromFormulaUntyped(p)
    case Not(True)          => False
    case Not(False)         => True
    case Not(p @ Var(_))    => !p
    case Not(p)             => fromFormulaUntyped(Not(fromFormulaUntyped(p)))

sealed trait CnfImplicits:
  given showCnf[C <: Cnf](using Emphasis): Show[C] = Show.show(toFormula(_).show)

  given stringToClause: Conversion[String, Clause] = fromFormula(_) match
    case clause: Clause => clause
    case _              => COr(Nil)
