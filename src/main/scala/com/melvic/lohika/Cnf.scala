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
    case Or(p, q, Nil) if isCnf(p)     => Or.flatten(Or.of(p, convertFormula(q)))
    case Or(p, q, r :: rs) if isCnf(p) => Or.flatten(Or.of(p, convertDisjunction(Or(q, r, rs))))
    case Or(p, q, rs) =>
      convertFormula(Or.flatten(Or(convertFormula(p), convertFormula(q), rs.map(convertFormula))))

  def convertConjunction: ToCnf[And] =
    case And(p, q, Nil) if isCnf(p)     => And.flatten(And.of(p, convertFormula(q)))
    case And(p, q, r :: rs) if isCnf(p) => And.flatten(And.of(p, convertConjunction(And(q, r, rs))))
    case And(p, q, rs) =>
      convertFormula(And.flatten(And(convertFormula(p), convertFormula(q), rs.map(convertFormula))))

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
