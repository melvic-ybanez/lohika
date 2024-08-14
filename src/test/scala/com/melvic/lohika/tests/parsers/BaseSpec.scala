package com.melvic.lohika.tests.parsers

import com.melvic.lohika.{Formula, Parser}
import fastparse.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BaseSpec extends AnyFlatSpec with should.Matchers:
  def parseSuccess(input: String, expected: Formula): Unit =
    Parser.parseFormula(input) should matchPattern:
      case Parsed.Success(`expected`, _) =>
