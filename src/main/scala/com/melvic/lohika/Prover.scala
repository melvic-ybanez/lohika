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
      case And(p, q) =>
        for
          pProof <- proveProposition(p)
          qProof <- proveProposition(q)
        yield Proof.derive(pProof :: qProof :: Nil, and)
//
//  def proveByConjElimination(formula: Formula): Prove =
//    @tailrec
//    def recurse(visited: Derivations, unvisited: Derivations): Result =
//      unvisited match
//        case Nil => Left(formula)
//        case Assumption(and @ And(p, q)) :: rest =>
//          val conjProofs = conjElimination(and).productIterator.toList
//          Derivations.findByFormula(conjProofs, formula).fold(
//            recurse(conjProofs ++ visited, rest)
//          )(Right.apply)
//        case Derivation()
//        case fm :: rest => recurse(fm :: visited, rest)
//
//  def conjElimination(and: And): (Derivation, Derivation) =
//    List(Proof.derive(and.p, Assumption(and)), Proof.derive(and.q, Assumption(and)))
