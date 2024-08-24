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
    case Or(And(ap, aq, _), q, Nil) => convertFormula((ap | q) & (aq | q))
    case Or(p: And, q, r :: rs)     => convertFormula(Or(convertFormula(p | q), r, rs))
    case Or(p, q: And, rs)          => convertFormula(Or(q, p, rs))
    case Or(p, q, rs) =>
      rs
        .find :
          case _: And => true
          case _      => false
        .fold {
          (isInCnf(p), rs) match
            case (true, Nil)      => Or.flatten(p | convertFormula(q))
            case (true, r :: rss) => Or.flatten(p | convertDisjunction(Or(q, r, rss)))
            case (_, _) =>
              convertFormula(
                Or.flatten(Or(convertFormula(p), convertFormula(q), rs.map(convertFormula)))
              )
        } { and =>
          convertFormula(Or(and, p, q :: rs.filterNot(_ == and)))
        }

  def convertConjunction: ToCnf[And] =
    case And(p, q, Nil) if isInCnf(p)     => And.flatten(p & convertFormula(q))
    case And(p, q, r :: rs) if isInCnf(p) => And.flatten(p & convertConjunction(And(q, r, rs)))
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
    case Not(p @ Var(_))    => !p
    case Not(p)             => convertFormula(Not(convertFormula(p)))
