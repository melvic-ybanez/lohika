package com.melvic.lohika.tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import cats.*
import cats.implicits.*
import com.melvic.lohika.Formatter
import com.melvic.lohika.Formatter.Format
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.formula.Formula.Quantification.forall

class PrettyPrintSpec extends AnyFlatSpec with should.Matchers with PrettyPrintGivens:
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

  "Implications" should "be separated by `=>`s" in:
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

  "Biconditionals" should "be separated by `<=>`s" in:
    ("A" <==> "B").show should be("A <=> B")

  "Universal Quantification" should "start with `A:`" in:
    forall("x", "y")("P" ==> "Q").show should be("A:x,y (P => Q)")
    forall("x")(forall("y")(("A" ==> "B") & "C")).show should be("A:x (A:y ((A => B) & C))")

trait PrettyPrintGivens:
  given Formatter with
    override def emphasize: Format = identity

    override def strong: Format = identity

    override def link(target: String): Format = identity

    override def itemNumber: String = ""
