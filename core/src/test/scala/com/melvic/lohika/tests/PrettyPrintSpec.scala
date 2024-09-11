package com.melvic.lohika.tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import cats.*
import cats.implicits.*
import com.melvic.lohika.formula.Formula.*

class PrettyPrintSpec extends AnyFlatSpec with should.Matchers:
  "A variable" should "print its name" in:
    Var("A").show should be("A")

  "Disjunctions" should "be separated by |s" in:
    Or.of("A", "B", "C").show should be("A | B | C")

  it should "wrapped components with lower or equal precedence levels in parens" in:
    Or.of("A", "B" | "C", "D").show should be("A | (B | C) | D")
    Or.of("A", "B" ==> "C", "D", "E" <==> "F").show should be(
      "A | (B => C) | D | (E <=> F)"
    )

  it should "not wrapped components with higher precedence levels in parens" in:
    Or.of("A", "B" & "C", "D").show should be("A | B & C | D")
    Or.of("A", !"B", "D", !"E").show should be("A | !B | D | !E")

  "Conjunctions" should "be separated by &s" in:
    And.of("A", "B", "C").show should be("A & B & C")

  it should "wrapped components with lower or equal precedence levels in parens" in:
    And.of("A", "B" & "C", "D").show should be("A & (B & C) & D")
    And.of("A", "B", "C" | "D").show should be("A & B & (C | D)")
    And.of("A", "B" ==> "C", "D", "E" <==> "F").show should be(
      "A & (B => C) & D & (E <=> F)"
    )
    ("A" | "B" ==> "C").show should be("A | (B => C)")

  it should "not wrapped components with higher precedence levels in parens" in:
    And.of("A", !"B", "D", !"E").show should be("A & !B & D & !E")

  "Implications" should "be separated by =>s" in:
    ("A" ==> "B").show should be("A => B")
    Imply.of("A", "B", "C").show should be("A => B => C")

  it should "wrapped components with lower or equal precedence levels in parens" in:
    Imply.of("A" ==> "B", "C").show should be("(A => B) => C")
    Imply.of("A", "B" ==> "C", "D").show should be("A => (B => C) => D")
    Imply.of("A", "B" <==> "C", "D", "E" <==> "F").show should be(
      "A => (B <=> C) => D => (E <=> F)"
    )

  it should "not wrapped components with higher precedence levels in parens" in:
    (("A" | "B") ==> "C").show should be("A | B => C")
    ("A" ==> ("B" & "C")).show should be("A => B & C")
    Imply.of("A", !"B", "D", !"E").show should be("A => !B => D => !E")
