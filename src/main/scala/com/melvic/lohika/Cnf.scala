package com.melvic.lohika

import com.melvic.lohika.Formula._

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
    case Or(l, p, Nil) if l.isLiteral     => Or.flatten(Or.of(l, convertFormula(p)))
    case Or(l, p, q :: rs) if l.isLiteral => Or.flatten(Or.of(l, convertDisjunction(Or(p, q, rs))))
    case Or(and: And, p, Nil)             => Or.flatten(Or.of(and, convertFormula(p)))
    case Or(and: And, p, q :: rs) => Or.flatten(Or.of(and, convertDisjunction(Or(p, q, rs))))
    case Or(p, q, rs) =>
      convertFormula(
        Or.flatten(Or(convertFormula(p), convertFormula(q), rs.map(convertFormula)))
      )

  def convertConjunction: ToCnf[And] =
    case And(p, q, rs) =>
      And.flatten(And(convertFormula(p), convertFormula(q), rs.map(convertFormula)))

  def convertImplication: ToCnf[Imply] =
    case Imply(p, q) => !convertFormula(p) | convertFormula(q)

  def convertBiconditional: ToCnf[Iff] =
    case Iff(p, q) => convertFormula(p ==> q) & convertFormula(q ==> p)

  def convertNot: ToCnf[Not] =
    case Not(Or(p, q, rs))  => convertFormula(And(!p, !q, rs.map(!_)))
    case Not(And(p, q, rs)) => convertFormula(Or(!p, !q, rs.map(!_)))
    case Not(Not(p))        => convertFormula(p)
    case Not(p @ Var(_))    => Not(p)
    case Not(p)             => convertFormula(Not(convertFormula(p)))
    case fm                 => fm
