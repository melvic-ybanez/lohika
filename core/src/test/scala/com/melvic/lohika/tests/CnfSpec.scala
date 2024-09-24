package com.melvic.lohika.tests

import BaseSpec.*
import com.melvic.lohika.formula.Cnf

class CnfSpec extends BaseSpec:
  "Disjunction" should "be flattened" in:
    "(A | B) | (C | (D | E))" ====> "A | B | C | D | E"
    "(A | C) | D" ====> "A | C | D"
    "(B | C) | D" ====> "B | C | D"

  it should "recursively convert its components to CNFs" in:
    "(A => B) | C | !!D" ====> "!A | B | C | D"
    "A | (!(B => A) => C) | C" ====> "A | !B | A | C | C"

  it should "distribute over conjunctions" in:
    "(A & B) | C" ====> "(A | C) & (B | C)"
    "A & B | C" ====> "(A | C) & (B | C)"
    "A | (B & C)" ====> "(A | B) & (A | C)"
    "(A & B) | C | D" ====> "(A | C | D) & (B | C | D)"
    "A | B | C | D & E" ====> "(A | B | C | D) & (A | B | C | E)"
    "P | (A & B & C)" ====> "(P | A) & (P | B) & (P | C)"

  "Conjunction" should "be flattened" in:
    "(A & B) & (C & (D & E))" ====> "A & B & C & D & E"

  it should "recursively convert its components to CNFs" in:
    "(A => B) & C & !!D" ====> "(!A | B) & C & D"
    "A & (!(B => A) => C) & C" ====> "A & (!B | A | C) & C"

  "p => q" should "become !p | q" in:
    "P => Q" ====> "!P | Q"

  "p <=> q" should "become (p => q) & (q => p) and further converted to CNF" in:
    "P <=> Q" ====> "(!P | Q) & (!Q | P)"

  "p <=> q <=> r" should "be the as (p <=> q) & (q <=> r)" in:
    "P <=> Q <=> R" ====> "(!P | Q) & (!Q | P) & (!Q | R) & (!R | Q)"

  "Implication" should "recursively convert its components to CNFs" in:
    "P => (Q & R)" ====> "(!P | Q) & (!P | R)"
    "(A => B) => (C => D)" ====> "(A | !C | D) & (!B | !C | D)"
    "P => (Q | R)" ====> "!P | Q | R"

  "!(p & q)" should "become !p | !q" in:
    "!(P & Q)" ====> "!P | !Q"
    "!((P => Q) & R)" ====> "(P | !R) & (!Q | !R)"

  "!(p | q)" should "become !p & q" in:
    "!(P | Q)" ====> "!P & !Q"
    "!((P => Q) | R)" ====> "P & !Q & !R"

  "!(p => q)" should "become p & !q" in:
    "!(P => Q)" ====> "P & !Q"

  "!((p | q) & (!q => r) => p => r)" should "become (p | q) & (q | r) & p & !r" in:
    "!((p | q) & (!q => r) => p => r)" ====> "(p | q) & (q | r) & p & !r"

  "Biconditional" should "recursively convert its components to CNFs" in:
    "P <=> (Q & R)" ====> "(!P | Q) & (!P | R) & (!Q | !R | P)"

  "Double negation" should "cancel out" in:
    "!!P" ====> "P"
    "!!!P" ====> "!P"
    "!!!!P" ====> "P"

  "All implications" should "be eliminated" in:
    assertFromInputStrings(
      "!((p | q) & (!q => r) => p => r)",
      "!(!((p | q) & (!(!q) | r)) | (!p | r))"
    ): (input, output) =>
      Cnf.eliminateImplications(input) should be(output)

    assertFromInputStrings("!(B => A) => C", "!(!(!B | A)) | C"): (input, output) =>
      Cnf.eliminateImplications(input) should be(output)

  "Negations" should "be moved inside" in:
    assertFromInputStrings("!(!(!B) & !A)", "!B | A"): (input, output) =>
      Cnf.moveNegationsInside(input) should be(output)

    assertFromInputStrings("A | !(!(!B) & !A) | C | C", "A | (!B | A) | C | C"):
      (input, output) => Cnf.moveNegationsInside(input) should be(output)
