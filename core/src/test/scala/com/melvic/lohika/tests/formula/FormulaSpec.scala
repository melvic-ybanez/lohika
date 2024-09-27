package com.melvic.lohika.tests.formula

import com.melvic.lohika.tests.BaseSpec
import com.melvic.lohika.tests.BaseSpec.====

class FormulaSpec extends BaseSpec:
  "Disjunction" should "support commutativity" in :
    "A | B | C" ==== "B | A | C"

  it should "support associativity" in :
    "(A | B) | C" ==== "A | B | C"
    "A | (B | C)" ==== "(A | B) | C"

  "Conjunction" should "be commutative" in :
    "A & B & C" ==== "C & B & A"

  it should "support associativity" in :
    "(A & B) & C" ==== "A & B & C"
    "A & (B & C)" ==== "(A & B) & C"

  it should "have lower precedence than disjunction" in:
    "(A | B) => (C | D)" ==== "A | B => C | D"
