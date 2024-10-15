package com.melvic.lohika.parsers

import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.formula.Formula.Quantified.BoundVars
import fastparse.{parse as fastParse, *}
import fastparse.MultiLineWhitespace.*

object Parser extends MetaParsing:
  def parseFormula(input: String): Parsed[Formula] =
    fastParse(input, formula(using _))

  def formula[$: P]: P[Formula] = P(iff)

  def quantified[$: P, Q <: Quantified](quantifier: String, make: Quantified.Make[Q])(using
      Q <:< Formula
  ): P[Formula] =
    P(quantifier ~/ firstOrderVar.rep(min = 1, sep = ",") ~ highestPrecedence).map:
      case (Seq(x, xs*), matrix) => make((x, xs.toList), matrix)

  def forall[$: P]: P[Formula] = quantified(Lexemes.Forall, Forall.apply)

  def thereExists[$: P]: P[Formula] = quantified(Lexemes.ThereExists, ThereExists.apply)

  def iff[$: P]: P[Formula] =
    imply
      .rep(min = 1, sep = Lexemes.Iff)
      .map(ps => Iff.fromList(ps.toList).getOrElse(PredicateApp.False))

  def imply[$: P]: P[Formula] = or
    .rep(min = 1, sep = Lexemes.Imply)
    .map:
      case Seq(p) => p
      case ps     => Imply.fromList(ps.toList)

  def or[$: P]: P[Formula] = and
    .rep(min = 1, sep = Lexemes.Or)
    .map(ps => Or.fromList(ps.toList))

  def and[$: P]: P[Formula] = highestPrecedence
    .rep(min = 1, sep = Lexemes.And)
    .map(ps => And.fromList(ps.toList))

  def not[$: P]: P[Formula] = P(Lexemes.Not ~ highestPrecedence).map(Not.apply)

  def grouping[$: P]: P[Formula] = P(Lexemes.LeftParen ~ formula ~ Lexemes.RightParen)

  def nullaryPred[$: P]: P[PredicateApp] =
    P(alphabetic(_.isUpper) ~ alphabetic(_ => true).rep(min = 0).!).map: (firstChar, rest) =>
      PredicateApp.nullary(firstChar + rest)

  def firstOrderVar[$: P]: P[Var] =
    P(alphabetic(_.isLower).rep(min = 1).!).map(Var.apply)

  def alphabetic[$: P](filter: Char => Boolean): P[String] =
    CharPred(c => Character.isAlphabetic(c) && filter(c)).!

  def predicate[$: P]: P[PredicateApp] = P(nullaryPred ~ args).map:
    case (PredicateApp.Nullary(name), args) => PredicateApp(name, args)

  def functionApp[$: P]: P[FunctionApp] = P(firstOrderVar ~ args).map:
    case (Var(name), args) => FunctionApp(name, args)

  def args[$: P]: P[List[Term]] = P("(" ~/ term.rep(min = 1, sep = ",") ~ ")").map(_.toList)

  def highestPrecedence[$: P]: P[Formula] = P(
    forall | thereExists | grouping | not | predicate | nullaryPred 
  )

  def trueConst[$: P]: P[True.type] =
    P(Lexemes.True).map(_ => True)

  def falseConst[$: P]: P[False.type] =
    P(Lexemes.False).map(_ => False)

  def constants[$: P]: P[True.type | False.type] = P(trueConst | falseConst)

  def term[$: P]: P[Term] = constants | functionApp | firstOrderVar
