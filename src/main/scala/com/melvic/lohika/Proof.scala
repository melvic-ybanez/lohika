package com.melvic.lohika

final case class Proof(assumptions: Assumptions, proposition: Formula, rule: Rule = None):
  def hasFormula(formula: Formula): Boolean =
    proposition == formula || assumptions.hasFormula(formula)

object Proof:
  def assume(formula: Formula): Proof =
    Proof(Assumptions.fromProofs(Proof(Assumptions.none, formula)), formula, "Assumption")

type Rule = "Assumption" | "&-introduction" | None.type
