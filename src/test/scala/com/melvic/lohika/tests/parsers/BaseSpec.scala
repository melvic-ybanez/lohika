package com.melvic.lohika.tests.parsers

import com.melvic.lohika.{Formula, Parser}
import fastparse.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BaseSpec extends AnyFlatSpec with should.Matchers:
  def parseSuccess(input: String, expected: Formula): Unit =
    Parser.parseFormula(input) should matchPattern:
      case Parsed.Success(`expected`, _) =>

  def assertEqualFormulae(input1: String, input2: String): Unit =
    (Parser.parseFormula(input1), Parser.parseFormula(input2)) should matchPattern:
      case (Parsed.Success(formula1, _), Parsed.Success(formula2, _)) if formula1 == formula2 =>
