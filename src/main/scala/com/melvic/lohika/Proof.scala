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
    proofs.exists:
      case Assumption(assumedFormula) => assumedFormula == formula
      case derivation: Derivation     => derivation.hasFormula(formula)

final case class Derivation(from: Derivations, conclusion: Formula):
  def hasFormula(formula: Formula): Boolean =
    conclusion == formula || Derivations.hasFormula(from, formula)

final case class Assumption(formula: Formula)
