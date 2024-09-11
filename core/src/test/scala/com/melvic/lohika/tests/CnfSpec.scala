package com.melvic.lohika.tests

import BaseSpec.*

class CnfSpec extends BaseSpec:
  "Disjunction" should "be flattened" in:
    "(A | B) | (C | (D | E))" ====> "A | B | C | D | E"

  it should "recursively convert its components to CNFs" in:
    "(A => B) | C | !!D" ====> "!A | B | C | D"
    "A | (!(B => A) => C) | C" ====> "A | !B | A | C | C"

  it should "distribute over conjunctions" in:
    "(A & B) | C" ====> "(A | C) & (B | C)"
    "A & B | C" ====> "(A | C) & (B | C)"
    "A | (B & C)" ====> "(B | A) & (C | A)"
    "(A & B) | C | D" ====> "(A | C | D) & (B | C | D)"
    "A | B | C | D & E" ====> "(D | A | B | C) & (E | A | B | C)"

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
    "P => (Q & R)" ====> "(Q | !P) & (R | !P)"
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

  "Biconditional" should "recursively convert its components to CNFs" in:
    "P <=> (Q & R)" ====> "(Q | !P) & (R | !P) & (!Q | !R | P)"

  "Double negation" should "cancel out" in:
    "!!P" ====> "P"
    "!!!P" ====> "!P"
    "!!!!P" ====> "P"
