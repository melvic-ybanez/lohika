package com.melvic.lohika.formula

import Formula._

object PrettyPrinter:
  final case class Style(apply: String => String)

  def prettyPrint(formula: Formula)(using parentPrecedence: Int = Precedence.Iff): String =
    given currentPrecedence: Int = precedence(formula)

    def prettyAssoc(p: Formula, q: Formula, rs: List[Formula], sep: String): String =
      val pqString = s"${prettyPrint(p)} $sep ${prettyPrint(q)}"
      if rs.isEmpty then pqString else s"$pqString $sep ${rs.map(prettyPrint).mkString(s" $sep ")}"

    val pretty = formula match
      case Var(name)     => name
      case Or(p, q, rs)  => prettyAssoc(p, q, rs, "|")
      case And(p, q, rs) => prettyAssoc(p, q, rs, "&")
      case Imply(p, q: Imply) =>
        s"${prettyPrint(p)} => ${prettyPrint(q)(using currentPrecedence - 1)}"
      case Imply(p, q)   => s"${prettyPrint(p)} => ${prettyPrint(q)}"
      case Iff(p, q, rs) => prettyAssoc(p, q, rs, "<=>")
      case Not(p)        => s"!${prettyPrint(p)}"
      case True          => "T"
      case False         => "F"

    if parentPrecedence >= currentPrecedence then s"(${pretty})" else pretty

  def precedence: Formula => Int =
    case _: Iff       => Precedence.Iff
    case _: Imply     => Precedence.Imply
    case _: Or        => Precedence.Or
    case _: And       => Precedence.And
    case _: Not       => Precedence.Not
    case _: Var       => Precedence.Var
    case True | False => Precedence.Var

  object Precedence:
    val Iff: Int = 1
    val Imply: Int = Iff + 1
    val Or: Int = Imply + 1
    val And: Int = Or + 1
    val Not: Int = And + 1
    val Var: Int = Not + 1
