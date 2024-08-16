package com.melvic.lohika

import Formula._

object Prover:
  type Result = Either[Formula, Proof]

  type Prove = Derivations ?=> Result

  def proveProposition(proposition: Formula): Prove =
    proveByAssumption(proposition).orElse:
      proposition match
        case and: And => proveByAndIntroduction(and)
        case _        => Left(proposition)

  def proveByAssumption(formula: Formula): Prove =
    Either.cond(
      Derivations.hasFormula(summon[Derivations], formula),
      Proof.assume(formula),
      formula
    )

  def proveByAndIntroduction(and: And): Prove =
    and match
      case And(p, q) =>
        for
          pProof <- proveProposition(p)
          qProof <- proveProposition(q)
        yield Proof.derive(pProof :: qProof :: Nil, and)
