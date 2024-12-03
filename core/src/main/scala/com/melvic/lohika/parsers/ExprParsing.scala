package com.melvic.lohika.parsers

import com.melvic.lohika.expression.Expression
import com.melvic.lohika.expression.Expression.*
import com.melvic.lohika.parsers.Parser.alphabetic
import fastparse.*
import fastparse.MultiLineWhitespace.*

private[parsers] trait ExprParsing:
  def expression[$: P]: P[Expression] = Parser.formula | Parser.term

  def term[$: P]: P[Term] = constants | functionApp | firstOrderVar

  def functionApp[$: P]: P[FunctionApp] = P(firstOrderVar ~ args).map:
    case (Var(name), args) => FunctionApp(name, args)

  def firstOrderVar[$: P]: P[Var] =
    P((alphabetic(_.isLower).rep(min = 1) ~ "_".? ~ CharPred(_.isDigit).rep(min = 0)).!)
      .map(Var.apply)

  def args[$: P]: P[List[Term]] =
    csvInParens[$, Term](term)

  def params[$: P]: P[List[Var]] =
    csvInParens(firstOrderVar)

  def csvInParens[$: P, A](item: => P[A]): P[List[A]] =
    P(Lexemes.LeftParen ~ item.rep(min = 1, sep = ",") ~ Lexemes.RightParen).map(_.toList)

  def trueConst[$: P]: P[True.type] =
    P(Lexemes.True).map(_ => True)

  def falseConst[$: P]: P[False.type] =
    P(Lexemes.False).map(_ => False)

  def namedConst[$: P]: P[Const] = (Lexemes.Const ~ firstOrderVar).map:
    case Var(name) => Const(name)

  def constants[$: P]: P[Const | True.type | False.type] = P(namedConst | trueConst | falseConst)
