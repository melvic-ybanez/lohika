package com.melvic.lohika.formula

import cats.Show
import com.melvic.lohika.Parser
import fastparse._

trait Implicits:
  given stringToFormula: Conversion[String, Formula] with
    override def apply(input: String): Formula = Parser.parseFormula(input) match
      case Parsed.Success(fm: Formula, _) => fm
      case _                              => throw new Error(s"Unable to parse $input")

  given show[A <: Formula]: Show[A] = Show.show(PrettyPrinter.prettyPrint)
