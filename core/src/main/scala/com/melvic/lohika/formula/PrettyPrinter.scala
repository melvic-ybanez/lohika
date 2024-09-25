package com.melvic.lohika.formula

import Formula.*
import com.melvic.lohika.formula.Formula.Quantification.QVars
import com.melvic.lohika.parsers.Lexemes

object PrettyPrinter:
  def prettyPrint(formula: Formula)(using parentPrecedence: Int = Precedence.Default): String =
    given currentPrecedence: Int = precedence(formula)

    def prettyFList(p: Formula, q: Formula, rs: List[Formula], sep: String): String =
      val pqString = s"${prettyPrint(p)} $sep ${prettyPrint(q)}"
      if rs.isEmpty then pqString else s"$pqString $sep ${rs.map(prettyPrint).mkString(s" $sep ")}"

    def prettyQuantification(quantifier: String, vars: List[Var], matrix: Formula): String =
      given Int = Precedence.Default
      s"$quantifier${vars.map(prettyPrint).mkString(",")} (${prettyPrint(matrix)})"

    val pretty = formula match
      case Var(name)     => name
      case Or(p, q, rs)  => prettyFList(p, q, rs, Lexemes.Or)
      case And(p, q, rs) => prettyFList(p, q, rs, Lexemes.And)
      case Imply(p, q: Imply) =>
        s"${prettyPrint(p)} ${Lexemes.Imply} ${prettyPrint(q)(using currentPrecedence - 1)}"
      case Imply(p, q)             => s"${prettyPrint(p)} ${Lexemes.Imply} ${prettyPrint(q)}"
      case Iff(p, q, rs)           => prettyFList(p, q, rs, Lexemes.Iff)
      case Not(p)                  => s"${Lexemes.Not}${prettyPrint(p)}"
      case True                    => Lexemes.True
      case False                   => Lexemes.False
      case Forall((x, xs), matrix) => prettyQuantification(Lexemes.Forall, x :: xs, matrix)
      case ThereExists((x, xs), matrix) =>
        prettyQuantification(Lexemes.ThereExists, x :: xs, matrix)

    if parentPrecedence >= currentPrecedence then
      s"${Lexemes.LeftParen}$pretty${Lexemes.RightParen}"
    else pretty

  def precedence: Formula => Int =
    case _: Iff         => Precedence.Iff
    case _: Imply       => Precedence.Imply
    case _: Or          => Precedence.Or
    case _: And         => Precedence.And
    case _: Not         => Precedence.Not
    case _: Var         => Precedence.Var
    case _: Forall      => Precedence.Forall
    case _: ThereExists => Precedence.ThereExists
    case True | False   => Precedence.Var

  object Precedence:
    val Default = 0
    val Iff: Int = Default + 1
    val Imply: Int = Iff + 1
    val Or: Int = Imply + 1
    val And: Int = Or + 1
    val Not: Int = And + 1
    val Var: Int = Not + 1
    val Forall: Int = Var
    val ThereExists: Int = Var
