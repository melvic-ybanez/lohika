package com.melvic.lohika.formula

import Formula.*

import scala.annotation.targetName
import scala.util.chaining.*

type Formula = Or | And | Imply | Iff | Var | Not | True.type | False.type

object Formula extends Implicits:
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
