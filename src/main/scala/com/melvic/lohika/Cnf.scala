package com.melvic.lohika

import com.melvic.lohika.Formula._

object Cnf:
  type ToCnf[A <: Formula] = A => Formula

  def fromFormula: ToCnf[Formula] =
    case or: Or       => fromDisjunction(or)
    case and: And     => fromConjunction(and)
    case imply: Imply => fromImplication(imply)
    case iff: Iff     => fromBiconditional(iff)
    case not: Not     => fromNot(not)
    case fm           => fm

  def fromDisjunction: ToCnf[Or] =
    // Identity Law: True (P | T === T)
    case or if or.components.contains(True) => True

    // Identity Law: Neutral Element (P | F === P)
    case or if or.components.contains(False) => Or.fromList(or.components.filterNot(_ == False))

    case or if isInCnf(or)          => or
    case Or(And(ap, aq, _), q, Nil) => fromFormula((ap | q) & (aq | q))
    case Or(p: And, q, r :: rs)     => fromFormula(Or(fromFormula(p | q), r, rs))
    case or if or.components.exists(isAnd) =>
      or.components
        .find(isAnd)
        .fold(or): and =>
          fromFormula(Or.fromList(and :: or.components.filterNot(_ == and)))
    case Or(p, q, rs) =>
      fromFormula(
        Or.flatten(Or(fromFormula(p), fromFormula(q), rs.map(fromFormula)))
      )

  def fromConjunction: ToCnf[And] =
    // Identity Law: False (P & F === F)
    case and if and.components.contains(False) => False

    // Identity Law: Neutral Element (P & T === P)
    case and if and.components.contains(True) => And.fromList(and.components.filterNot(_ == True))

    case and if isInCnf(and) => and
    case And(p, q, rs) =>
      fromFormula(And.flatten(And(fromFormula(p), fromFormula(q), rs.map(fromFormula))))

  def fromImplication: ToCnf[Imply] =
    case Imply(p, q) => fromFormula(!fromFormula(p) | fromFormula(q))

  def fromBiconditional: ToCnf[Iff] =
    case Iff(p, q, Nil) => fromFormula(p ==> q) & fromFormula(q ==> p)
    case Iff(p, q, rs) =>
      val iffs = rs.foldLeft(List(p <==> q)):
        case (iffs @ (Iff(p, q, _) :: _), r) => (q <==> r) :: iffs
      fromFormula(And.fromList(iffs.reverse))

  def fromNot: ToCnf[Not] =
    case Not(Or(p, q, rs))  => fromFormula(And(!p, !q, rs.map(!_)))
    case Not(And(p, q, rs)) => fromFormula(Or(!p, !q, rs.map(!_)))
    case Not(Not(p))        => fromFormula(p)
    case Not(True)          => False
    case Not(False)         => True
    case Not(p @ Var(_))    => !p
    case Not(p)             => fromFormula(Not(fromFormula(p)))
