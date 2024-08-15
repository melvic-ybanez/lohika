package com.melvic.lohika

final case class Proof(assumptions: Assumptions, proposition: Formula):
  def hasFormula(formula: Formula): Boolean =
    proposition == formula || assumptions.hasFormula(formula)

