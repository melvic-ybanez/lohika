package com.melvic.lohika

import com.melvic.lohika.Formula.{And, Imply, Not, Or}

object Cnf:
  type ToCnf[A <: Formula] = A => Formula

  def convertFormula: ToCnf[Formula] =
    case or: Or       => convertDisjunction(or)
    case and: And     => convertConjunction(and)
    case imply: Imply => convertImplication(imply)
    case fm           => fm

  def convertDisjunction: ToCnf[Or] =
    Formula.flatten

  def convertConjunction: ToCnf[And] =
    Formula.flatten

  def convertImplication: ToCnf[Imply] =
    case Imply(p, q) => Or.of(Not(p), q)
