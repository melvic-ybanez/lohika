package com.melvic.lohika.tests

class FormulaSpec extends BaseSpec {
  "Disjunction" should "support commutativity" in :
    assertEqualFormulae("A | B | C", "B | A | C")

  it should "support associativity" in :
    assertEqualFormulae("(A | B) | C", "A | B | C")
    assertEqualFormulae("A | (B | C)", "(A | B) | C")

  "Conjunction" should "be commutative" in :
    assertEqualFormulae("A & B & C", "C & B & A")

  it should "support associativity" in :
    assertEqualFormulae("(A & B) & C", "A & B & C")
    assertEqualFormulae("A & (B & C)", "(A & B) & C")

  it should "have lower precedence than disjunction" in:
    assertEqualFormulae("(A | B) => (C | D)", "A | B => C | D")
}
