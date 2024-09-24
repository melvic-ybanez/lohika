package com.melvic.lohika.formula

import cats.*
import cats.implicits.*
import com.melvic.lohika.formula.Cnf.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.Formatter

type Cnf = CAnd | Clause

object Cnf extends CnfGivens:
  type ConvertWithFrom[F <: Formula] = Endo[Formula] => F => Formula

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

  def eliminateBiconditionals: Endo[Formula] =
    case Iff(p, q, Nil) => eliminateBiconditionals((p ==> q) & (q ==> p))
    case Iff(p, q, rs) =>
      val iffs = rs.foldLeft(List(p <==> q)):
        case (iffs @ (Iff(_, q, _) :: _), r) => (q <==> r) :: iffs
      eliminateBiconditionals(And.fromList(iffs.reverse))
    case imply: Imply => fromImplyWith(eliminateBiconditionals)(imply)
    case or: Or       => fromOrWith(eliminateBiconditionals)(or)
    case and: And     => fromAndWith(eliminateBiconditionals)(and)
    case not: Not     => fromNotWith(eliminateBiconditionals)(not)
    case fm           => fm

  def eliminateImplications: Endo[Formula] =
    case Imply(p, q) => eliminateImplications(!p | q)
    case or: Or      => fromOrWith(eliminateImplications)(or)
    case and: And    => fromAndWith(eliminateImplications)(and)
    case not: Not    => fromNotWith(eliminateImplications)(not)
    case fm          => fm

  def moveNegationsInside: Endo[Formula] =
    case Not(Or(p, q, rs))    => moveNegationsInside(And(!p, !q, rs.map(!_)))
    case Not(And(p, q, rs))   => moveNegationsInside(Or(!p, !q, rs.map(!_)))
    case notNot @ Not(Not(_)) => moveNegationsInside(simplifyNegations(notNot))
    case not: Not             => fromNotWith(moveNegationsInside)(not)
    case or: Or               => fromOrWith(moveNegationsInside)(or)
    case and: And             => fromAndWith(moveNegationsInside)(and)
    case fm                   => fm

  def distributeOrOverAnds: Endo[Formula] =
    case Or(p, And(ap, aq, ars), Nil) =>
      fromAndWith(distributeOrOverAnds)(And(p | ap, p | aq, ars.map(p | _)))
    case Or(And(ap, aq, ars), q, Nil) =>
      fromAndWith(distributeOrOverAnds)(And(ap | q, aq | q, ars.map(_ | q)))
    case Or(p, and: And, r :: rs) => distributeOrOverAnds(Or(distributeOrOverAnds(p | and), r, rs))
    case Or(and: And, q, r :: rs) => distributeOrOverAnds(Or(distributeOrOverAnds(and | q), r, rs))
    case Or(p, q, (and: And) :: rs) =>
      distributeOrOverAnds(Or(p, distributeOrOverAnds(q | and), rs))
    case Or(p, q, r :: rs) => distributeOrOverAnds(p | distributeOrOverAnds(Or(q, r, rs)))
    case and: And          => fromAndWith(distributeOrOverAnds)(and)
    case not: Not          => fromNotWith(distributeOrOverAnds)(not)
    case fm                => fm

  def simplifyNegations: Endo[Formula] =
    case Not(Not(p))     => simplifyNegations(p)
    case Not(True)       => False
    case Not(False)      => True
    case Not(p @ Var(_)) => !p
    case not: Not        => not
    case or: Or          => fromOrWith(simplifyNegations)(or)
    case and: And        => fromAndWith(simplifyNegations)(and)
    case fm              => fm

  def flattenConjunctions: Endo[Formula] =
    case and: And =>
      val flattened = and.components.map(flattenOrsAndAnds)
      flattened.tail.foldLeft(flattened.head):
        case (And(p, q, rs), and: And) => And(p, q, rs ++ and.components)
        case (And(p, q, rs), fm)       => And(p, q, rs ++ List(fm))
        case (fm, And(p, q, rs))       => And(fm, p, q :: rs)
        case (fm1, fm2)                => fm1 & fm2
    case or: Or => fromOrWith(flattenConjunctions)(or)
    case fm     => fm

  def flattenDisjunctions: Endo[Formula] =
    case or: Or =>
      val flattened = or.components.map(flattenOrsAndAnds)
      flattened.tail.foldLeft(flattened.head):
        case (Or(p, q, rs), or: Or) => Or(p, q, rs ++ or.components)
        case (Or(p, q, rs), fm)     => Or(p, q, rs ++ List(fm))
        case (fm, Or(p, q, rs))     => Or(fm, p, q :: rs)
        case (fm1, fm2)             => fm1 | fm2
    case and: And => fromAndWith(flattenDisjunctions)(and)
    case fm       => fm

  def flattenOrsAndAnds: Endo[Formula] =
    case and: And => flattenConjunctions(and)
    case or: Or   => flattenDisjunctions(or)
    case fm       => fm

  def fromImplyWith: ConvertWithFrom[Imply] = f =>
    case Imply(p, q) => f(p) ==> f(q)

  def fromOrWith: ConvertWithFrom[Or] = f =>
    case Or(p, q, rs) => Or(f(p), f(q), rs.map(f))

  def fromAndWith: ConvertWithFrom[And] = f =>
    case And(p, q, rs) => And(f(p), f(q), rs.map(f))

  def fromNotWith: ConvertWithFrom[Not] = f =>
    case Not(p) => Not(f(p))

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
