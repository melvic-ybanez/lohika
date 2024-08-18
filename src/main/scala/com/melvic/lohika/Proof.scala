package com.melvic.lohika

type Proof = Assumption | Derivation
type Derivations = List[Proof]

object Proof:
  def assume(formula: Formula): Proof =
    Derivation(Assumption(formula) :: Nil, formula)

  def derive(from: Derivations, conclusion: Formula): Proof =
    Derivation(from, conclusion)

object Derivations:
  def hasFormula(proofs: Derivations, formula: Formula): Boolean =
    findByFormula(proofs, formula).nonEmpty

  def findByFormula(proofs: Derivations, formula: Formula): Option[Proof] =
    proofs.find:
      case Assumption(assumedFormula) => assumedFormula == formula
      case derivation: Derivation => derivation.hasFormula(formula)

final case class Derivation(from: Derivations, conclusion: Formula):
  def findByFormula(formula: Formula): Option[Proof] =
    if conclusion == formula then Some(this)
    else Derivations.findByFormula(from, formula)

  def hasFormula(formula: Formula): Boolean =
    findByFormula(formula).nonEmpty

final case class Assumption(formula: Formula)
