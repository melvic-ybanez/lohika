package com.melvic.lohika

import com.melvic.lohika.Formula.Or

object Cnf:
  type ToCnf[A <: Formula] = A => Formula

  def convert: ToCnf[Formula] =
    case or: Or => convertDisjunction(or)
    case fm => fm

  def convertDisjunction: ToCnf[Or] =
    Or.flatten
