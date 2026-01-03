package com.melvic.lohika.core.proofs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EquivalenceProofSpec extends BaseProofSpec:
  "Commutative laws" should "be provable" in:
    contradiction("P | Q = Q | P")
    contradiction("P & Q = Q & P")

    // commutativity of the law of excluded middle. This isn't really part of commutativity laws.
    // just added it here since I spent some time debugging an issue related to it.
    contradiction("P | !P = !P | P")

  "Idempotent laws" should "be provable" in:
    contradiction("P | P = P")
    contradiction("P & P = P")

  "Double negation" should "be cancel out" in:
    contradiction("!(!P) = P")

  "Associative laws" should "be provable" in:
    contradiction("(P | Q) | R = P | (Q | R)")
    contradiction("(P & Q) & R = P & (Q & R)")

  "Distributive laws" should "be provable" in:
    contradiction("P | (Q & R) = (P | Q) & (P | R)")
    contradiction("P & (Q | R) = (P & Q) | (P & R)")

  "De Morgan's laws" should "be provable" in:
    contradiction("!(P & Q) = !P | !Q")
    contradiction("!(P | Q) = !P & !Q")

  "Implication" should "correspond to its algter" in:
    contradiction("P -> Q = !P | Q")

  it should "correspond to the correct contrapositive form" in:
    contradiction("P -> Q = !Q -> !P")
