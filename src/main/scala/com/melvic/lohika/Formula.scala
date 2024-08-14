package com.melvic.lohika

import com.melvic.lohika.Formula.{And, Iff, Imply, Or}

sealed trait Formula

object Formula:
  final case class Var(name: String) extends Formula
  final case class Or(p: Formula, q: Formula) extends Formula
  final case class And(p: Formula, q: Formula) extends Formula
  final case class Imply(p: Formula, q: Formula) extends Formula
  final case class Iff(p: Formula, q: Formula) extends Formula
  final case class Not(p: Formula) extends Formula
  case object True extends Formula
  case object False extends Formula

  object Or:
    def fromSeq(components: Seq[Formula]): Or =
      (components.tail.foldLeft(components.head)(Or.apply): @unchecked) match
        case or: Or => or

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

    def |(that: Formula): Or = Or(formula, that)

    def ==>(that: Formula): Imply = Imply(formula, that)

    def <==>(that: Formula): Iff = Iff(formula, that)
