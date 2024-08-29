package com.melvic.lohika

import com.melvic.lohika.Formula.*
import fastparse.Parsed

import scala.annotation.targetName
import scala.collection.immutable.Nil as And
import scala.util.chaining.*

type Formula = Or | And | Imply | Iff | Var | Not | True.type | False.type

object Formula:
  final case class Var(name: String)
  final case class Or(p: Formula, q: Formula, rs: List[Formula]) extends FList
  final case class And(p: Formula, q: Formula, rs: List[Formula]) extends FList
  final case class Imply(p: Formula, q: Formula)
  final case class Iff(p: Formula, q: Formula, rs: List[Formula]) extends FList
  final case class Not(p: Formula)
  case object True
  case object False

  type Property = Formula => Boolean

  sealed trait FList:
    def p: Formula

    def q: Formula

    def rs: List[Formula]

    def components: List[Formula] = p :: q :: rs

  def isInCnf: Property =
    case fm if isLiteral(fm) => true
    case and: And            => and.components.forall(and => isInCnf(and) && !isAnd(and))
    case or: Or              => or.components.forall(isLiteral)
    case _                   => false

  def isLiteral: Property =
    case v: Var               => true
    case Not(Var(_))          => true
    case fm if isConstant(fm) => true
    case _                    => false

  def isConstant: Property =
    case True | False => true
    case _            => false

  def isAnd: Property =
    case _: And => true
    case _      => false

  def isOr: Property =
    case _: Or => true
    case _     => false

  def isClause: Property = fm => isOr(fm) || isLiteral(fm)

  def flatten[A <: FList](make: (Formula, Formula, List[Formula]) => Formula)(
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

  object Or:
    def of(p: Formula, q: Formula, rs: Formula*): Or =
      Or(p, q, rs.toList)

    def fromList: List[Formula] => Formula =
      case Nil          => False
      case p :: Nil     => p
      case p :: q :: rs => Or(p, q, rs)

    def flatten: Formula => Formula =
      Formula.flatten(Or.apply) { case or: Or => or }

  object And:
    def of(p: Formula, q: Formula, rs: Formula*): And =
      And(p, q, rs.toList)

    def fromList: List[Formula] => Formula =
      case Nil          => True
      case p :: Nil     => p
      case p :: q :: rs => And(p, q, rs)

    def flatten: Formula => Formula =
      Formula.flatten(And.apply) { case and: And => and }

  object Iff:
    def of(p: Formula, q: Formula, rs: Formula*): Iff =
      Iff(p, q, rs.toList)

    def fromList: List[Formula] => Option[Formula] =
      case p :: Nil     => Some(p)
      case p :: q :: rs => Some(Iff(p, q, rs))
      case _            => None

    def flatten: Formula => Formula =
      Formula.flatten(Iff.apply) { case iff: Iff => iff }

  object Imply:
    def of(p: Formula, qs: Formula*): Imply =
      Imply.fromList(p :: qs.toList)

    def fromList(components: List[Formula]): Imply =
      (components.reduceRight(Imply.apply): @unchecked) match
        case imply: Imply => imply

  given stringToFormula: Conversion[String, Formula] with
    override def apply(input: String): Formula = Parser.parseFormula(input) match
      case Parsed.Success(fm: Formula, _) => fm
      case _                              => throw new Error(s"Unable to parse $input")

  extension (self: Formula)
    @targetName("and")
    def &(other: Formula): And = And.of(self, other)

    @targetName("or")
    def |(other: Formula): Or = Or.of(self, other)

    @targetName("implies")
    def ==>(other: Formula): Imply = Imply(self, other)

    @targetName("ifAndOnlyIf")
    def <==>(other: Formula): Iff = Iff.of(self, other)

    @targetName("not")
    def unary_! : Not = Not(self)

    @targetName("entails")
    def ===(other: Formula): Boolean =
      def hasSameComps(
          selfFList: Formula,
          otherFList: Formula,
          flatten: Formula => Formula
      ): Boolean =
        def flattenComponents(fList: Formula): List[Formula] = (flatten(fList): @unchecked) match
          case flatFList: FList => flatFList.components

        val selfComps = flattenComponents(selfFList)
        val otherComps = flattenComponents(otherFList)

        def containsAll(comps1: List[Formula], comps2: List[Formula]): Boolean =
          comps2.forall(fm => comps1.exists(_ === fm))

        (selfComps.size == otherComps.size) && containsAll(selfComps, otherComps) && containsAll(
          otherComps,
          selfComps
        )

      (self, other) match
        case (or1: Or, or2: Or)     => hasSameComps(or1, or2, Or.flatten)
        case (and1: And, and2: And) => hasSameComps(and1, and2, And.flatten)
        case (iff1: Iff, iff2: Iff) => hasSameComps(iff1, iff2, Iff.flatten)
        case (thisCnf, thatCnf)     => thisCnf == thatCnf

  object Precedence:
    val Iff: Int = 1
    val Imply: Int = Iff + 1
    val Or: Int = Imply + 1
    val And: Int = Or + 1
    val Not: Int = And + 1
    val Var: Int = Not + 1
