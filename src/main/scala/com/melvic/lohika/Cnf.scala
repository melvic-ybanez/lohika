package com.melvic.lohika

import com.melvic.lohika.Formula.{And, Iff, Imply, Not, Or}

object Cnf:
  type ToCnf[A <: Formula] = A => Formula

  def convertFormula: ToCnf[Formula] =
    case or: Or       => convertDisjunction(or)
    case and: And     => convertConjunction(and)
    case imply: Imply => convertImplication(imply)
    case iff: Iff     => convertBiconditional(iff)
    case not: Not     => convertNot(not)
    case fm           => fm

  def convertDisjunction: ToCnf[Or] =
    Formula.flatten

  def convertConjunction: ToCnf[And] =
    Formula.flatten

  def convertImplication: ToCnf[Imply] =
    case Imply(p, q) => !convertFormula(p) | convertFormula(q)

  def convertBiconditional: ToCnf[Iff] =
    case Iff(p, q) => convertFormula(p ==> q) & convertFormula(q ==> p)

  def convertNot: ToCnf[Not] =
    case Not(Or(p, q, rs))  => And(!p, !q, rs.map(!_))
    case Not(And(p, q, rs)) => Or(!p, !q, rs.map(!_))
    case Not(Not(p))        => p
    case fm                 => fm
