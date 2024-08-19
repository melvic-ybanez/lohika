package com.melvic.lohika

import com.melvic.lohika.Formula.*

import scala.util.chaining.*

type Formula = Var | Or | And | Imply | Iff | Not | True.type | False.type

object Formula:
  final case class Var(name: String)
  final case class Or(p: Formula, q: Formula, rs: List[Formula])
  final case class And(p: Formula, q: Formula, rs: List[Formula])
  final case class Imply(p: Formula, q: Formula)
  final case class Iff(p: Formula, q: Formula)
  final case class Not(p: Formula)
  case object True
  case object False

  object Or:
    def of(p: Formula, q: Formula, rs: Formula*): Or =
      Or(p, q, rs.toList)

    def flatten: Formula => Formula =
      def toList: Formula => List[Formula] =
        case Or(p, q, rs) => toList(p) ++ toList(q) ++ rs.flatMap(toList)
        case fm           => List(fm)

      {
        case or: Or =>
          toList(or).pipe:
            case h :: t :: rest => Or(h, t, rest)
        case fm => fm
      }

  object And:
    def of(p: Formula, q: Formula, rs: Formula*): And =
      And(p, q, rs.toList)

    def flatten: Formula => Formula =
      def toList: Formula => List[Formula] =
        case And(p, q, rs) => toList(p) ++ toList(q) ++ rs.flatMap(toList)
        case fm            => List(fm)

      {
        case and: And =>
          toList(and).pipe:
            case h :: t :: rest => And(h, t, rest)
        case fm => fm
      }

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
