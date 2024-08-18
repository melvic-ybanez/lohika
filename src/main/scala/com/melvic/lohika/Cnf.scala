package com.melvic.lohika

import com.melvic.lohika.Formula.{And, Or}

object Cnf:
  type ToCnf[A <: Formula] = A => Formula

  def convert: ToCnf[Formula] =
    case or: Or   => convertDisjunction(or)
    case and: And => convertConjunction(and)
    case fm       => fm

  def convertDisjunction: ToCnf[Or] =
    Formula.flatten

  def convertConjunction: ToCnf[And] =
    Formula.flatten
