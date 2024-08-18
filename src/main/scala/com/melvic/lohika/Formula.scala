package com.melvic.lohika

import com.melvic.lohika.Formula._
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

  def flatten: Formula => Formula =
    def flattenContents(fm: Formula, make: (Formula, Formula, List[Formula]) => Formula): Formula =
      toList(fm).pipe:
        case h :: t :: rest => make(h, t, rest)

    def contentToList(p: Formula, q: Formula, rs: List[Formula]): List[Formula] =
      toList(p) ++ toList(q) ++ rs.flatMap(toList)

    def toList: Formula => List[Formula] =
      case Or(p, q, rs)  => contentToList(p, q, rs)
      case And(p, q, rs) => contentToList(p, q, rs)
      case fm            => List(fm)

    {
      case or: Or   => flattenContents(or, Or.apply)
      case and: And => flattenContents(and, And.apply)
      case fm       => fm
    }

  object Or:
    def of(p: Formula, q: Formula, rs: Formula*): Or =
      Or(p, q, rs.toList)

  object And:
    def of(p: Formula, q: Formula, rs: Formula*): And =
      And(p, q, rs.toList)

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

    def isIsomorphicTo(that: Formula): Boolean =
      // TODO: Ignore the order of the elements (e.g. A | B | C === A | C | B)
      Cnf.convert(formula) == Cnf.convert(that)
