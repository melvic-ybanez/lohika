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
      components.tail.foldLeft(components.head)(Or.apply) match
        case or: Or => or
        case _      => Or(False, False)

  object And:
    def fromSeq(components: Seq[Formula]): And =
      components.tail.foldLeft(components.head)(And.apply) match
        case and: And => and
        case _        => And(False, False)

  object Imply:
    def fromSeq(components: Seq[Formula]): Imply =
      components.reduceRight(Imply.apply) match
        case imply: Imply => imply

  given stringToVar: Conversion[String, Var] with
    override def apply(input: String): Var = Var(input)

  extension (formula: Formula)
    def &(that: Formula): And = And(formula, that)

    def |(that: Formula): Or = Or(formula, that)

    def ==>(that: Formula): Imply = Imply(formula, that)

    def <==>(that: Formula): Iff = Iff(formula, that)

