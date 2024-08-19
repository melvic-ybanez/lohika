package com.melvic.lohika

import com.melvic.lohika.Formula.*

import scala.util.chaining.*

type Formula = Var | Or | And | Imply | Iff | Not | True.type | False.type

object Formula:
  sealed trait Chain {
    def p: Formula
    def q: Formula
    def rs: List[Formula]
  }

  final case class Var(name: String)
  final case class Or(p: Formula, q: Formula, rs: List[Formula]) extends Chain
  final case class And(p: Formula, q: Formula, rs: List[Formula]) extends Chain
  final case class Imply(p: Formula, q: Formula)
  final case class Iff(p: Formula, q: Formula)
  final case class Not(p: Formula)
  case object True
  case object False

  def flatten[A <: Chain](make: (Formula, Formula, List[Formula]) => Formula)(
      filter: PartialFunction[Formula, A]
  ): Formula => Formula =
    def toList: Formula => List[Formula] =
      case fm if filter.isDefinedAt(fm) =>
        val chain = filter(fm)
        toList(chain.p) ++ toList(chain.q) ++ chain.rs.flatMap(toList)
      case fm => List(fm)
  
    {
      case fm if filter.isDefinedAt(fm) =>
        toList(fm).pipe:
          case h :: t :: rest => make(h, t, rest)
      case fm => fm
    }
  
  object Or:
    def of(p: Formula, q: Formula, rs: Formula*): Or =
      Or(p, q, rs.toList)

    def flatten: Formula => Formula =
      Formula.flatten(Or.apply) { case or: Or => or }

  object And:
    def of(p: Formula, q: Formula, rs: Formula*): And =
      And(p, q, rs.toList)

    def flatten: Formula => Formula =
      Formula.flatten(And.apply) { case and: And => and }

  object Imply:
    def fromSeq(components: Seq[Formula]): Imply =
      (components.reduceRight(Imply.apply): @unchecked) match
        case imply: Imply => imply

  given stringToVar: Conversion[String, Var] with
    override def apply(input: String): Var = Var(input)

  extension (formula: Formula)
    def &(that: Formula): And = And.of(formula, that)

    def |(that: Formula): Or = Or.of(formula, that)

    def ==>(that: Formula): Imply = Imply(formula, that)

    def <==>(that: Formula): Iff = Iff(formula, that)

    def unary_! : Not = Not(formula)

    def isIsomorphicTo(that: Formula): Boolean =
      // TODO: Ignore the order of the elements (e.g. A | B | C === A | C | B)
      Cnf.convertFormula(formula) == Cnf.convertFormula(that)
