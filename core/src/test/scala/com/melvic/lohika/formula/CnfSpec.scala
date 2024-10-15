package com.melvic.lohika.formula

import com.melvic.lohika.formula.Formula
import FormulaMappingSupport.{====>, FormulaMapper}
import com.melvic.lohika.BaseSpec

class CnfSpec extends BaseSpec with FormulaMappingSupport:
  "Disjunction" should "be flattened" in:
    "(A | B) | (C | (D | E))" ====> "A | B | C | D | E"
    "(A | C) | D"             ====> "A | C | D"
    "(B | C) | D"             ====> "B | C | D"

  it should "recursively convert its components to CNFs" in:
    "(A => B) | C | !!D"       ====> "!A | B | C | D"
    "A | (!(B => A) => C) | C" ====> "A | !B | A | C | C"

  it should "distribute over conjunctions" in:
    "(A & B) | C"       ====> "(A | C) & (B | C)"
    "A & B | C"         ====> "(A | C) & (B | C)"
    "A | (B & C)"       ====> "(A | B) & (A | C)"
    "(A & B) | C | D"   ====> "(A | C | D) & (B | C | D)"
    "A | B | C | D & E" ====> "(A | B | C | D) & (A | B | C | E)"
    "P | (A & B & C)"   ====> "(P | A) & (P | B) & (P | C)"

  "Conjunction" should "be flattened" in:
    "(A & B) & (C & (D & E))" ====> "A & B & C & D & E"

  it should "recursively convert its components to CNFs" in:
    "(A => B) & C & !!D"       ====> "(!A | B) & C & D"
    "A & (!(B => A) => C) & C" ====> "A & (!B | A | C) & C"

  "P => Q" should "become !P | Q" in:
    "P => Q" ====> "!P | Q"

  "P <=> Q" should "become (P => Q) & (Q => P) and further converted to CNF" in:
    "P <=> Q" ====> "(!P | Q) & (!Q | P)"

  "P <=> Q <=> R" should "be the as (P <=> Q) & (Q <=> R)" in:
    "P <=> Q <=> R" ====> "(!P | Q) & (!Q | P) & (!Q | R) & (!R | Q)"

  "Implication" should "recursively convert its components to CNFs" in:
    "P => (Q & R)"         ====> "(!P | Q) & (!P | R)"
    "(A => B) => (C => D)" ====> "(A | !C | D) & (!B | !C | D)"
    "P => (Q | R)"         ====> "!P | Q | R"

  "!(P & Q)" should "become !P | !Q" in:
    "!(P & Q)"        ====> "!P | !Q"
    "!((P => Q) & R)" ====> "(P | !R) & (!Q | !R)"

  "!(P | Q)" should "become !P & Q" in:
    "!(P | Q)"        ====> "!P & !Q"
    "!((P => Q) | R)" ====> "P & !Q & !R"

  "!(P => Q)" should "become P & !Q" in:
    "!(P => Q)" ====> "P & !Q"

  "!((P | Q) & (!Q => R) => P => R)" should "become (P | Q) & (Q | R) & P & !R" in:
    "!((P | Q) & (!Q => R) => P => R)" ====> "(P | Q) & (Q | R) & P & !R"

  "Biconditional" should "recursively convert its components to CNFs" in:
    "P <=> (Q & R)" ====> "(!P | Q) & (!P | R) & (!Q | !R | P)"

  "Double negation" should "cancel out" in:
    "!!P"   ====> "P"
    "!!!P"  ====> "!P"
    "!!!!P" ====> "P"

  override given formulaMapper: FormulaMapper =
    FormulaMapper(Formula.toCnfRaw)
