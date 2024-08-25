package com.melvic.lohika.tests

import com.melvic.lohika.Formula.*
import com.melvic.lohika.{Formula, Parser}
import fastparse.*
import org.scalatest.Assertions.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BaseSpec extends AnyFlatSpec with should.Matchers:
  def assertEqualFormulae(input1: String, input2: String): Unit =
    (Parser.parseFormula(input1), Parser.parseFormula(input2)) match
      case (Parsed.Success(formula1, _), Parsed.Success(formula2, _)) =>
        assert(formula1 === formula2)
      case _ => assert(false)
