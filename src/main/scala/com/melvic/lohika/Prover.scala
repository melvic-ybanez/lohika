package com.melvic.lohika

import Formula._

object Prover:
  type Result = Option[Proof]

  def proveProposition(assumptions: Assumptions, proposition: Formula): Result =
    proposition match
      case variable: Var => proveVar(assumptions, variable)
      case _             => None

  def proveVar(assumptions: Assumptions, variable: Var): Result =
    if assumptions.hasFormula(variable) then
      Some(Proof(Assumptions.fromFormula(variable), variable))
    else None
