package com.melvic.lohika

import Formula._

object Prover:
  type Result = Option[Proof]

  type Prove = Assumptions ?=> Result

  def proveProposition(proposition: Formula): Prove =
    proveByAssumption(proposition).orElse:
      proposition match
        case and: And => proveByAndIntroduction(and)
        case _        => None

  def proveByAssumption(formula: Formula): Prove =
    Option.when(summon[Assumptions].hasFormula(formula)):
      Proof(Assumptions.fromFormulae(formula), formula, "Assumption")

  def proveByAndIntroduction(and: And): Prove =
    and match
      case And(p, q) =>
        for
          pProof <- proveProposition(p)
          qProof <- proveProposition(q)
        yield Proof(Assumptions.fromProofs(pProof, qProof), and, "&-introduction")
