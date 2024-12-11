package com.melvic.lohika.core.formula

import cats.*
import cats.implicits.*
import com.melvic.lohika.core.BaseSpec.{assertFromInputStrings, parseFailure}
import com.melvic.lohika.core.Formatter
import com.melvic.lohika.core.formula.Formula
import com.melvic.lohika.core.formula.FormulaMappingSupport.FormulaMapper
import com.melvic.lohika.core.parsers.Parser
import fastparse.Parsed

import scala.annotation.targetName

trait FormulaMappingSupport:
  given formulaMapper: FormulaMapper

object FormulaMappingSupport:
  final case class FormulaMapper(map: Endo[Formula])

  type Assert = (FormulaMapper, Formatter) ?=> Unit

  extension (self: String)
    @targetName("assertTransformedFromStrings")
    def ====>(other: String): Assert =
      assertFromInputStrings(self, other)(assertTransformedFormulas)

    @targetName("assertTransformedFromStringAndFormula")
    def ====>(other: Formula): Assert =
      Parser.parseFormula(self) match
        case Parsed.Success(formula, _)  => assertTransformedFormulas(formula, other)
        case Parsed.Failure(label, _, _) => parseFailure(self, label)

    def assertTransformedFormulas(actual: Formula, expected: Formula): Assert =
      val transformed = summon[FormulaMapper].map(actual)
      assert(
        transformed == expected,
        show"\n$actual has the expected transformed form: $expected. Got: $transformed"
      )
