package com.melvic.lohika

import Formula._

object Prover:
  type Result = Option[Proof]

  def proveProposition(assumptions: Assumptions, proposition: Formula): Result =
    assumptionRule(assumptions, proposition)

  def assumptionRule(assumptions: Assumptions, formula: Formula): Result =
    if assumptions.hasFormula(formula) then
      Some(Proof(Assumptions.fromFormulae(formula), formula))
    else None
