package com.melvic.lohika

import com.melvic.lohika.Formula._
import scala.util.chaining.*

type Formula = Var | Or | And | Imply | Iff | Not | True.type | False.type

object Formula:
  final case class Var(name: String)
  final case class Or(p: Formula, q: Formula, rs: List[Formula])
  final case class And(p: Formula, q: Formula)
  final case class Imply(p: Formula, q: Formula)
  final case class Iff(p: Formula, q: Formula)
  final case class Not(p: Formula)
  case object True
  case object False

  object Or:
    def of(p: Formula, q: Formula, rs: Formula*): Or =
      Or(p, q, rs.toList)
      
    def flattenedList: Formula => List[Formula] =
      case Or(p, q, rs) => flattenedList(p) ++ flattenedList(q) ++ rs.flatMap(flattenedList)
      case fm        => List(fm)

    def flatten: Formula => Formula =
      case or @ Or(p, q, rs) => flattenedList(or).pipe:
        case h :: t :: rest => Or(h, t, rest)
      case fm => fm

  object And:
    def fromSeq(components: Seq[Formula]): And =
      (components.tail.foldLeft(components.head)(And.apply): @unchecked) match
        case and: And => and

  object Imply:
    def fromSeq(components: Seq[Formula]): Imply =
      (components.reduceRight(Imply.apply): @unchecked) match
        case imply: Imply => imply

  given stringToVar: Conversion[String, Var] with
    override def apply(input: String): Var = Var(input)

  extension (formula: Formula)
    def &(that: Formula): And = And(formula, that)

    def |(that: Formula): Or = Or(formula, that, Nil)

    def ==>(that: Formula): Imply = Imply(formula, that)

    def <==>(that: Formula): Iff = Iff(formula, that)

    def isIsomorphicTo(that: Formula): Boolean =
      // TODO: Ignore the order of the elements (e.g. A | B | C === A | C | B)
      Cnf.convert(formula) == Cnf.convert(that)
