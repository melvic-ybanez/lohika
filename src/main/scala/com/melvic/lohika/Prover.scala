package com.melvic.lohika

import Formula.*

import scala.annotation.tailrec

object Prover:
  type Result = Either[Formula, Proof]

  type Prove = Derivations ?=> Result

  def proveProposition(proposition: Formula): Prove =
    proveByAssumption(proposition).orElse:
      proposition match
        case and: And => proveByConjIntro(and)
        case _        => Left(proposition)

  def proveByAssumption(formula: Formula): Prove =
    Either.cond(
      Derivations.hasFormula(summon[Derivations], formula),
      Proof.assume(formula),
      formula
    )

  def proveByConjIntro(and: And): Prove =
    and match
      case And(p, q, rs) =>
        for
          pProof <- proveProposition(p)
          qProof <- proveProposition(q)
          rsProofs <- rs.map(proveProposition).partitionMap(identity) match
            case (Nil, proofs) => Right(proofs)
            case (fm :: _, _)  => Left(fm)
        yield Proof.derive(pProof :: qProof :: rsProofs, and)
