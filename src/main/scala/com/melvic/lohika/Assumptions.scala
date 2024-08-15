package com.melvic.lohika

final case class Assumptions(underlying: List[Proof]):
  def hasFormula(formula: Formula): Boolean =
    underlying.exists(_.hasFormula(formula))


object Assumptions:
  def fromProofs(assumptions: Proof*): Assumptions =
    Assumptions(assumptions.toList)

  def none: Assumptions = Assumptions(Nil)

  def fromFormula(formula: Formula): Assumptions =
    Assumptions.fromProofs(Proof(Assumptions.none, formula))
