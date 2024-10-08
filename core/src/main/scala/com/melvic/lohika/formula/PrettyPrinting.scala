package com.melvic.lohika.formula

import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.formula.Formula.Quantified.BoundVars
import com.melvic.lohika.formula.Formula.Precedence.noParensIfEqual
import com.melvic.lohika.parsers.Lexemes

private[formula] trait PrettyPrinting:
  def prettyPrint(formula: Formula)(using parentPrecedence: Int = Precedence.Default): String =
    given currentPrecedence: Int = precedence(formula)

    def prettyFList(p: Formula, q: Formula, rs: List[Formula], sep: String): String =
      val pqString = s"${prettyPrint(p)} $sep ${prettyPrint(q)}"
      if rs.isEmpty then pqString else s"$pqString $sep ${rs.map(prettyPrint).mkString(s" $sep ")}"

    def prettyQuantified(quantifier: String, vars: BoundVars, matrix: Formula): String =
      quantifier + (vars._1 :: vars._2)
        .map { variable =>
          // we don't wrap bound vars with parens when declaring them, so we give the
          // parent precedence a lower value (default)
          prettyPrint(variable)(using Precedence.Default)
        }
        .mkString(",") + prettyPrint(matrix)(using noParensIfEqual)

    val pretty = formula match
      case Var(name)          => name
      case Or(p, q, rs)       => prettyFList(p, q, rs, Lexemes.Or)
      case And(p, q, rs)      => prettyFList(p, q, rs, Lexemes.And)
      case Imply(p, q: Imply) =>
        // Implications are right-associative, so let's not add parens around the codomain if
        // it is another implication.
        s"${prettyPrint(p)} ${Lexemes.Imply} ${prettyPrint(q)(using noParensIfEqual)}"
      case Imply(p, q)               => s"${prettyPrint(p)} ${Lexemes.Imply} ${prettyPrint(q)}"
      case Iff(p, q, rs)             => prettyFList(p, q, rs, Lexemes.Iff)
      case Not(p)                    => s"${Lexemes.Not}${prettyPrint(p)}"
      case True                      => Lexemes.True
      case False                     => Lexemes.False
      case Forall(vars, matrix)      => prettyQuantified(Lexemes.Forall, vars, matrix)
      case ThereExists(vars, matrix) => prettyQuantified(Lexemes.ThereExists, vars, matrix)
      case Predicate(name, args) =>
        given Int = Precedence.Default
        s"$name(${args.map(prettyPrint).mkString(", ")})"

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
    case _: Predicate   => Precedence.Predicate
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
    val ThereExists: Int = Forall
    val Predicate: Int = Var

    /**
     * Use this if the current precedence and its parent are equal, and you want to force Lohika to
     * avoid auto-wrapping the current scope in parens.
     */
    def noParensIfEqual(using precedence: Int): Int =
      precedence - 1
