package com.melvic.lohika.parsers

import com.melvic.lohika.expression.Expression.*
import com.melvic.lohika.formula.Formula.FunctionApp
import com.melvic.lohika.parsers.Parser.alphabetic
import fastparse.*
import fastparse.MultiLineWhitespace.*

private[parsers] trait ExprParsing:
  def term[$: P]: P[Term] = constants | functionApp | firstOrderVar

  def functionApp[$: P]: P[FunctionApp] = P(firstOrderVar ~ args).map:
    case (Var(name), args) => FunctionApp(name, args)

  def firstOrderVar[$: P]: P[Var] =
    P((alphabetic(_.isLower).rep(min = 1) ~ "_".? ~ CharPred(_.isDigit).rep(min = 0)).!)
      .map(Var.apply)

  def args[$: P]: P[List[Term]] = P("(" ~ term.rep(min = 1, sep = ",") ~ ")").map(_.toList)

  def trueConst[$: P]: P[True.type] =
    P(Lexemes.True).map(_ => True)

  def falseConst[$: P]: P[False.type] =
    P(Lexemes.False).map(_ => False)

  def constApp[$: P]: P[Const] = (Lexemes.Const ~ firstOrderVar).map:
    case Var(name) => Const(name)

  def constants[$: P]: P[Const | True.type | False.type] = P(constApp | trueConst | falseConst)
