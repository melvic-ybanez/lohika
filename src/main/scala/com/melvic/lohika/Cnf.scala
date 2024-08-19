package com.melvic.lohika

import com.melvic.lohika.Formula.{And, Iff, Imply, Not, Or}

object Cnf:
  type ToCnf[A <: Formula] = A => Formula

  def convertFormula: ToCnf[Formula] =
    case or: Or       => convertDisjunction(or)
    case and: And     => convertConjunction(and)
    case imply: Imply => convertImplication(imply)
    case iff: Iff     => convertBiconditional(iff)
    case fm           => fm

  def convertDisjunction: ToCnf[Or] =
    Formula.flatten

  def convertConjunction: ToCnf[And] =
    Formula.flatten

  def convertImplication: ToCnf[Imply] =
    case Imply(p, q) => !p | q

  def convertBiconditional: ToCnf[Iff] =
    case Iff(p, q) => convertFormula(p ==> q) & convertFormula(q ==> p)
